{-# LANGUAGE OverloadedStrings #-}
module Main
       ( main -- :: IO ()
       ) where

import Data.ByteString.Char8 as S
import Control.Monad (liftM)
import System.Posix.User (getEffectiveUserID)
import Database.LevelDB
import System.Exit
import Data.Maybe (fromJust)

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

main :: IO ()
main = do
  dbname <- (("/tmp/leveldb_hs_test-"++) . show) `liftM` getEffectiveUserID
  say $ "creating db " ++ dbname
  let opts = def {dbCreateIfMissing = True, dbErrorIfExists = True}
  destroy dbname opts

  runResourceT $ do
    db <- open dbname opts
    say "stats"
    s <- property db DBStats
    say_ (fromJust s)

    say "put"
    putBS db def "foo" "hello"
    putBS db def "box" "c"

    say "get"
    x <- getBS db def "foo"
    say_ (show x)
    x <- getBS db def "box"
    say_ (show x)

    say "snapshot"
    withSnapshot db $ \snap -> do
      deleteBS db def "foo"
      x <- getBS db def{readSnapshot = Just snap} "foo"
      say_ (show x)
      x <- getBS db def "foo"
      say_ (show x)
    return ()

  say "repair"
  repair dbname def

  say "stats"
  runResourceT $ do
    db <- open dbname def
    s <- property db DBStats
    say_ (fromJust s)

  say "end"

say_ :: MonadIO m => String -> m ()
say_ = liftIO . Prelude.putStrLn

say :: MonadIO m => String -> m ()
say x = say_ $ "===== " ++ x ++ " ====="
