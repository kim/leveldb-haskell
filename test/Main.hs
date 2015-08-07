module Main (main) where

import qualified Test.Issue29   as Issue29
import qualified Test.Streaming as Streaming
import           Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "Tests" [ Streaming.tests, Issue29.tests ]
