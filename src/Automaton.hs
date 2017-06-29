{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Automaton (showGraph, isValidJson) where

import qualified Data.ByteString.Lazy as BL
import           Control.Arrow
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Array           as A
import qualified Data.Array.Unboxed   as AU
import           Data.Char
import           Data.List
import qualified Data.Map.Lazy        as M
import           Data.Maybe
import qualified Data.Set             as S
import           Data.Word8
import           Debug.Trace
import           Text.Printf

-- Define the graph of states and transitions

data TransitionGraph l
  = ConsumeRanges l [(Word8, Word8)] l
  | Then           (TransitionGraph l) (TransitionGraph l)
  | Optional       (TransitionGraph l) l
  | OrElse         (TransitionGraph l) (TransitionGraph l) l
  | Many           (TransitionGraph l)
  | CommaSeparated l (TransitionGraph l) l
  | InnerValue                         l l
  deriving (Show, Eq)

word8FromChar :: Char -> Word8
word8FromChar = fromIntegral . ord

char :: Char -> TransitionGraph ()
char c = charRange c c

wordRange :: Word8 -> Word8 -> TransitionGraph ()
wordRange w1 w2 = ConsumeRanges () [(w1, w2)] ()

charRange :: Char -> Char -> TransitionGraph ()
charRange c1 c2 = charRanges [(c1, c2)]

charRanges :: [(Char, Char)] -> TransitionGraph ()
charRanges = consumeRanges . map (word8FromChar *** word8FromChar)
  where
  consumeRanges r = ConsumeRanges () r ()

charChoices :: String -> TransitionGraph ()
charChoices = charRanges . map (\c -> (c, c))

literal :: String -> TransitionGraph ()
literal cs = foldr1 Then $ map char cs

digit :: TransitionGraph ()
digit = charRange '0' '9'

digit19 :: TransitionGraph ()
digit19 = charRange '1' '9'

leadingSign :: TransitionGraph ()
leadingSign = char '-'

integerPart :: TransitionGraph ()
integerPart = zero `orElse` nonzero
  where
  zero    = char '0'
  nonzero = digit19 `Then` Many digit

fractionalPart :: TransitionGraph ()
fractionalPart = char '.'
          `Then` digit
          `Then` Many digit

exponentPart :: TransitionGraph ()
exponentPart = charChoices "eE"
  `Then` Optional (charChoices "+-") ()
  `Then` digit
  `Then` Many digit

number :: TransitionGraph ()
number = Optional leadingSign    ()
  `Then` integerPart
  `Then` Optional fractionalPart ()
  `Then` Optional exponentPart   ()

string :: TransitionGraph ()
string = char '"'
  `Then` Many quotedChar
  `Then` char '"'

quotedChar :: TransitionGraph ()
quotedChar = escapedChar
  `orElse` charRanges [(' ', pred '"'), (succ '"', pred '\\'), (succ '\\', chr 0x7f)]
  `orElse` (utf8Without1Continuation `Then` anyUtf8Continuation)

anyUtf8Continuation :: TransitionGraph ()
anyUtf8Continuation = wordRange 0x80 0xbf

utf8Without1Continuation :: TransitionGraph ()
utf8Without1Continuation
  =         wordRange 0xc2 0xdf
  `orElse` (wordRange 0xe0 0xe0 `Then` wordRange 0xa0 0xbf)
  `orElse` (utf8Without2Continuations `Then` anyUtf8Continuation)

utf8Without2Continuations :: TransitionGraph ()
utf8Without2Continuations
  =         wordRange 0xe1 0xef
  `orElse` (wordRange 0xf0 0xf0 `Then` wordRange 0x90 0xbf)
  `orElse` (wordRange 0xf1 0xf3 `Then` anyUtf8Continuation)
  `orElse` (wordRange 0xf4 0xf4 `Then` wordRange 0x80 0x8f)

escapedChar :: TransitionGraph ()
escapedChar = char '\\'
  `Then` (charChoices "\"\\/bfnrt" `orElse` unicodeEscape)

unicodeEscape :: TransitionGraph ()
unicodeEscape = char 'u'
  `Then` hexChar
  `Then` hexChar
  `Then` hexChar
  `Then` hexChar
  where
  hexChar = charRanges [('0', '9'), ('A', 'F'), ('a', 'f')]

object :: TransitionGraph ()
object = char '{'
  `Then` CommaSeparated ()
            (string `Then` char ':' `Then` value) ()
  `Then` char '}'

array :: TransitionGraph ()
array = char '['
  `Then` CommaSeparated () value ()
  `Then` char ']'

topLevelValue :: TransitionGraph ()
topLevelValue = object
       `orElse` array

value :: TransitionGraph ()
value    = literal "true"
  `orElse` literal "false"
  `orElse` literal "null"
  `orElse` number
  `orElse` string
  `orElse` InnerValue () ()

orElse :: TransitionGraph () -> TransitionGraph () -> TransitionGraph ()
orElse g1 g2 = OrElse g1 g2 ()

--

labelStates :: TransitionGraph () -> TransitionGraph Int
labelStates g0 = evalState (go g0) 0
  where
  nextLabel = do
    l <- get
    put $ l+1
    return l

  go (ConsumeRanges () ranges ()) = ConsumeRanges
                                            <$> nextLabel <*> pure ranges
                                                                <*> nextLabel

  go (Then g1 g2)             = Then           <$> go g1 <*> go g2
  go (Optional g ())          = Optional       <$> go g            <*> nextLabel
  go (OrElse g1 g2 ())        = OrElse         <$> go g1 <*> go g2 <*> nextLabel
  go (Many g)                 = Many           <$> go g
  go (CommaSeparated () g ()) = CommaSeparated <$> nextLabel
                                                         <*> go g <*> nextLabel
  go (InnerValue       () ()) = InnerValue     <$> nextLabel       <*> nextLabel

startLabel :: TransitionGraph l -> l
startLabel (ConsumeRanges l _ _)  = l
startLabel (Then     g _)         = startLabel g
startLabel (Optional g _)         = startLabel g
startLabel (OrElse   g _ _)       = startLabel g
startLabel (Many     g)           = startLabel g
startLabel (CommaSeparated l _ _) = l
startLabel (InnerValue     l _)   = l

finalLabel :: TransitionGraph l -> l
finalLabel (ConsumeRanges _ _ l)  = l
finalLabel (Then     _ g)         = finalLabel g
finalLabel (Optional _ l)         = l
finalLabel (OrElse   _ _ l)       = l
finalLabel (Many     g)           = startLabel g
finalLabel (CommaSeparated _ _ l) = l
finalLabel (InnerValue _ l)       = l

allLabels :: TransitionGraph a -> [a]
allLabels = execWriter . go
  where
  go (ConsumeRanges l1 _ l2)  = tell [l1,l2]
  go (Then g1 g2)             = go g1    >> go g2
  go (Optional g l)           = tell [l] >> go g
  go (OrElse g1 g2 l)         = tell [l] >> go g1    >> go g2
  go (Many g)                 = go g
  go (CommaSeparated l1 g l2) = tell [l1, l2] >> go g
  go (InnerValue l1 l2)       = tell [l1, l2]

automaton :: TransitionGraph Int
automaton = labelStates topLevelValue

data TransitionEdge
  = CharConsumingTransition Int [(Word8, Word8)] Int
  | InternalTransition Int String Int
  | FinalState         Int
  deriving (Show, Eq, Ord)

mapTarget :: (Int -> Int) -> TransitionEdge -> TransitionEdge
mapTarget f (CharConsumingTransition  s r t) = CharConsumingTransition  s r (f t)
mapTarget f (InternalTransition       s l t) = InternalTransition       s l (f t)
mapTarget f (FinalState                   l) = FinalState l

getSource :: TransitionEdge -> Int
getSource (CharConsumingTransition  s _ _) = s
getSource (InternalTransition       s _ _) = s
getSource (FinalState               s)     = s

nodesWithOneOutgoingTransitionWhichIsInternal :: [(Int, Int)]
nodesWithOneOutgoingTransitionWhichIsInternal
  = [ (s, t)
    | s <- allLabels automaton
    , let outgoingTransitions = [ te | te <- transitionEdgesBeforeCollapse
                                     , getSource te == s ]
    , length outgoingTransitions == 1
    , InternalTransition _ _ t <- outgoingTransitions
    ]

collapsedTarget :: Int -> Int
collapsedTarget n = case lookup n nodesWithOneOutgoingTransitionWhichIsInternal of
  Nothing -> n
  Just n' -> collapsedTarget n'

transitionEdges :: [TransitionEdge]
transitionEdges = map (mapTarget collapsedTarget) transitionEdgesBeforeCollapse

transitionEdgesBeforeCollapse :: [TransitionEdge]
transitionEdgesBeforeCollapse = execWriter $ do
    go automaton
    tell [FinalState (finalLabel automaton)]
  where
  go (ConsumeRanges l1 ranges l2) = tell [CharConsumingTransition l1 ranges l2]

  go (Then g1 g2)       = do go g1
                             tell [InternalTransition (finalLabel g1) "->" (startLabel g2)]
                             go g2

  go (Optional g l)     = do go g
                             tell [InternalTransition (startLabel g) "?" l]
                             tell [InternalTransition (finalLabel g) "^" l]

  go (OrElse g1 g2 l)   = do go g1
                             go g2
                             tell [InternalTransition (startLabel g1) "||" (startLabel g2)]
                             tell [InternalTransition (finalLabel g1) "V" l]
                             tell [InternalTransition (finalLabel g2) "V" l]

  go (Many g)           = do go g
                             tell [InternalTransition (finalLabel g) "*" (startLabel g)]

  go (CommaSeparated l1 g l2)
                        = do go g
                             tell [InternalTransition l1  "{}" l2]
                             tell [InternalTransition l1 "~{}" (startLabel g)]
                             tell [InternalTransition (finalLabel g) "EOF" l2]
                             tell [CharConsumingTransition (finalLabel g)
                                                           [(0x2c, 0x2c)]
                                                           (startLabel g)]

  go (InnerValue l1 l2) = tell [CharConsumingTransition l1 [(0,0)] l2]

relabelling :: Int -> Maybe Int
relabelling = flip lookup $ go S.empty [] 0 [0]
  where
  go _            result      _         []     = result
  go reachedSoFar resultSoFar nextLabel (n:ns)
    | n `S.member` reachedSoFar = go reachedSoFar resultSoFar nextLabel ns
    | otherwise = go (S.insert n reachedSoFar) ((n, nextLabel) : resultSoFar) (nextLabel + 1)
        $ reverse [ t
                  | Just es <- [M.lookup n externalTransitionsBySource]
                  , t <- [ t
                         | CharConsumingTransition _ _ t <- es
                         ]
                  ] ++ ns

relabelledTransitionEdges :: [TransitionEdge]
relabelledTransitionEdges = execWriter $ mapM_ tellRelabelled externalTransitionEdges
  where
  tellRelabelled e = maybe (return ()) (tell . return) $ relabelled e

  relabelled (CharConsumingTransition  s r t) = CharConsumingTransition  <$> relabelling s <*> pure r <*> relabelling t
  relabelled (FinalState               s)     = FinalState <$> relabelling s
  relabelled _ = error "internal edge"

externalTransitionEdges :: [TransitionEdge]
externalTransitionEdges = S.toList $ S.fromList $ concat $ M.elems externalTransitionsBySource

externalTransitionsBySource :: M.Map Int [TransitionEdge]
externalTransitionsBySource = M.fromListWith (++)
  $  [ (s, [e])
     | e@(CharConsumingTransition s _ _) <- transitionEdges
     ]
  ++ [ (s, [e])
     | e@(FinalState s) <- transitionEdges
     ]
  ++ [ (s, [ case e of CharConsumingTransition  _ r t -> CharConsumingTransition  s r t
                       FinalState               _     -> FinalState               s
                       _ -> error "internal edge found"

           | e <- case M.lookup t0 externalTransitionsBySource of
                    Nothing -> error ("node " ++ show t0 ++ " not found")
                    Just es -> es
           ])
     | InternalTransition s _ t0 <- transitionEdges
     ]

showGraph :: IO ()
showGraph = do
  writeFile "nodes.gv" $ execWriter $ do
    tell "digraph G {"
    mapM_ showTransitionEdge relabelledTransitionEdges
    tell "}"
  writeFile "nodes-all.gv" $ execWriter $ do
    tell "digraph G {"
    mapM_ showTransitionEdge transitionEdges
    tell "}"
  writeFile "automaton.txt" $ show makeAutomaton

  where
  showTransitionEdge (CharConsumingTransition l1 ranges l2)
    = tell $ printf "n%d -> n%d [label=\"%s\"]\n" l1 l2 (showRanges ranges)

  showTransitionEdge (InternalTransition l1 label l2)
    = tell $ printf "n%d -> n%d [label=\"%s\",style=\"dotted\",arrowhead=\"onormal\"]\n" l1 l2 label

  showTransitionEdge (FinalState l)
    = tell $ printf "n%d [shape=\"box\"]\n" l

showRanges :: [(Word8, Word8)] -> String
showRanges [] = "ERROR: EMPTY RANGE"
showRanges rs = intercalate "," $ map showRange rs
  where
  dblQuote = fromIntegral $ ord '"'
  bckSlash = fromIntegral $ ord '\\'

  chr8 w
    | 0x21 <= w && w <= 0x7e && w /= dblQuote && w /= bckSlash = show $ chr $ fromIntegral w
    | otherwise = printf "0x%02x" w

  showRange (w1, w2)
    | w1 == w2 = chr8 w1
    | otherwise = chr8 w1 ++ "-" ++ chr8 w2

data Automaton = Automaton
  { aTransitionsTable :: !(AU.Array (Word8, Word8) Word8)
    -- first  index is state
    -- second index is the next character
    -- entry  is the next state
    --
    -- special cases: state 0x00 is the start state;
    -- OTOH if an entry says 0x00 this means failure, as we never return to the start state otherwise
    --
    -- Also there are two transitions representing an inner value
    -- (at one point these were nodes 15 -> 16 and 48 -> 49).
    -- These are recorded against character 0x00, which is never legal in the input.
    --
    -- Therefore (a) immediately fail if there is a 0x00 character.
    -- (b) if the parse fails at one of these states (15 or 48) then
    -- remember where you are and try starting again. If this succeeds
    -- then treat that like parsing a 0x00.

  , aFinishState      :: {-# UNPACK #-} !Word8
    -- If the parse finishes in this state then done. If there are more
    -- characters after reaching this state then it fails.

  , aFirstValueWithinArray :: {-# UNPACK #-} !Word8
    -- At this state, we are within an array at the very start, so expect an inner value

  , aSubsequentValueWithinArray :: {-# UNPACK #-} !Word8
    -- At this state, we are within an array after a comma, so expect an inner value

  , aValueWithinObject :: {-# UNPACK #-} !Word8
    -- At this state, we are within an object, so expect an inner value
  } deriving (Show, Eq)

makeAutomaton :: Automaton
makeAutomaton = Automaton
  { aTransitionsTable = transitionsTable
  , aFinishState = let
      -- Parse an empty array '[]'
      state0 = 0
      state1 = transitionsTable AU.! (state0, _bracketleft)
      state2 = transitionsTable AU.! (state1, _bracketright)
      in state2
  , aFirstValueWithinArray = let
      -- Parse into an array '['
      state0 = 0
      state1 = transitionsTable AU.! (state0, _bracketleft)
      in state1
  , aSubsequentValueWithinArray = let
      -- Parse into an array '[0,'
      state0 = 0
      state1 = transitionsTable AU.! (state0, _bracketleft)
      state2 = transitionsTable AU.! (state1, _0)
      state3 = transitionsTable AU.! (state2, _comma)
      in state3
  , aValueWithinObject = let
      -- Parse into an object '{"":'
      state0 = 0
      state1 = transitionsTable AU.! (state0, _braceleft)
      state2 = transitionsTable AU.! (state1, _quotedbl)
      state3 = transitionsTable AU.! (state2, _quotedbl)
      state4 = transitionsTable AU.! (state3, _colon)
      in state4
  }
  where
  maxLabel = fromMaybe (error "no labels") $ maximum [ relabelling n | n <- allLabels automaton ]
  lbub@(lb, ub) = ((0,0), (fromIntegral maxLabel, 0xff))

  transitionsTable
    | 0xff < maxLabel = error "too many nodes"
    | otherwise = AU.array
      lbub
      [ ((currentState, nextByte), nextState)
      | (currentState, nextByte) <- AU.range lbub
      , let nextState = fromMaybe 0 $ listToMaybe
              [ fromIntegral t
              | CharConsumingTransition s rs t <- relabelledTransitionEdges
              , fromIntegral s == currentState
              , let withinCharRange (b0,b1) = b0 <= nextByte && nextByte <= b1
              , any withinCharRange rs
              ]
      ]

data AutomatonState
  = AutomatonState {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 ![Word8]
                                                               -- ^ stack of containing states
                                      -- ^^^^^^^^^^^^^^^^^^^^^ current state
                -- ^^^^^^^^^^^^^^^^^^^^^ 0x01 if still parsing
  deriving (Show, Eq)

alreadyFailed :: AutomatonState
alreadyFailed = AutomatonState 0x00 0 []

isValidJson :: BL.ByteString -> Bool
isValidJson bs = case finalState of
    AutomatonState ap s [] | s == aFinishState makeAutomaton
                           , ap == 0x01 -> True
    _                                   -> False

  where
  finalState = BL.foldl' step (AutomatonState 0x01 0 []) bs

  step s@(AutomatonState stillParsing currentState stack) nextByte
    = if

      | stillParsing == 0x00 -> alreadyFailed
      | nextByte     == 0x00 -> alreadyFailed

      | parseFailed ->
          if   currentState == aFirstValueWithinArray       makeAutomaton
            || currentState == aSubsequentValueWithinArray  makeAutomaton
            || currentState == aValueWithinObject           makeAutomaton
          then if nextStateInner == 0
                  then alreadyFailed
                  else AutomatonState 0x01 nextStateInner (currentState : stack)
          else alreadyFailed

      | parseFinished -> case stack of
          []     -> AutomatonState 0x01 nextState []
          (s:ss) -> AutomatonState 0x01 poppedState ss
            where
            poppedState = aTransitionsTable makeAutomaton AU.! (s, 0x00)

      | otherwise -> AutomatonState 0x01 nextState stack

    where
    {-# INLINE nextState #-}
    nextState      = aTransitionsTable makeAutomaton AU.! (currentState, nextByte)
    nextStateInner = aTransitionsTable makeAutomaton AU.! (0,            nextByte)
    parseFailed   = nextState == 0
    parseFinished = nextState == aFinishState makeAutomaton
