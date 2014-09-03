{-# LANGUAGE OverloadedStrings #-}

-- | Demo custom comparator

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Default
import           Database.LevelDB
import qualified Database.LevelDB.Streaming as S


customComparator :: Comparator
customComparator = Comparator compare

main :: IO ()
main = runResourceT $ do
    db <- open "/tmp/lvlcmptest"
               defaultOptions{ createIfMissing = True
                             , comparator = Just customComparator
                             }

    put db def "zzz" ""
    put db def "yyy" ""
    put db def "xxx" ""

    withIterator db def $ \iter -> liftIO $
            S.toList (S.entrySlice iter S.AllKeys S.Asc)
        >>= print

    return ()
