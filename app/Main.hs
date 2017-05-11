{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO as T
import qualified Automaton

testEvent :: BL.ByteString
testEvent = BL.fromStrict $ T.encodeUtf8 "{\"stream\":\"actuals-stream\",\"submitter\":{\"type\":\"other\",\"description\":\"redactedredac\"},\"driverActivities\":[{\"driverActivity\":{\"journey\":{\"headcode\":\"1A01\",\"description\":\"redactedredactedredactedredactedredactedredacte\"},\"activity\":[{\"arrivalTime\":null,\"sequence\":1,\"tiploc\":\"REDACTE\",\"stop\":true,\"departureTime\":\"2016-06-09T18:22:28.000000000000Z\"},{\"arrivalTime\":\"2016-06-09T18:24:43.000000000000Z\",\"sequence\":2,\"tiploc\":\"REDACTE\",\"stop\":true,\"departureTime\":\"2016-06-09T18:25:51.000000000000Z\"},{\"arrivalTime\":\"2016-06-09T18:26:58.000000000000Z\",\"sequence\":3,\"tiploc\":\"REDACT\",\"stop\":true,\"departureTime\":\"2016-06-09T18:28:08.000000000000Z\"},{\"arrivalTime\":\"2016-06-09T18:29:57.000000000000Z\",\"sequence\":4,\"tiploc\":\"REDACTE\",\"stop\":true,\"departureTime\":null}]},\"activityUserId\":\"521be60a-02f2-4892-b468-c17d9c1c4fcf\"}],\"submissionTime\":\"2016-06-09T18:36:45.831486000000Z\",\"type\":\"driverActivityLogged\"}"

data AnyJSON = AnyJSON
  deriving (Show, Eq)

instance FromJSON AnyJSON where
  parseJSON _ = pure AnyJSON

main :: IO ()
main = defaultMain
  [ bgroup "aeson"
    [ bench "testEvent" $ whnf (decode :: BL.ByteString -> Maybe AnyJSON) testEvent
    ]
  , bgroup "json-validator/Automaton"
    [ bench "testEvent" $ whnf Automaton.isValidJson testEvent
    ]
  ]
