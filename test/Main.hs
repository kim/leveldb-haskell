module Main (main) where

import           Control.Monad  (unless)
import           System.Exit    (exitFailure)
import           System.IO      (BufferMode (..), hSetBuffering, stderr, stdout)

import qualified Test.Basic     as Basic
import qualified Test.Streaming as Streaming
import           Test.Tasty


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    pass <- sequence [ Basic.tests ]
    unless (and pass) exitFailure

    defaultMain $ testGroup "Tests" [ Streaming.tests ]
