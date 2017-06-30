module Main (main) where

import           Automaton
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      contents <- BL.readFile file
      unless (isValidJson contents) $
        exitWith (ExitFailure 1)
    _ -> die "Usage: json-test <file>"
