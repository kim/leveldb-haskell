module Main (main) where

import qualified Test.Streaming as Streaming
import           Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "Tests" [ Streaming.tests ]
