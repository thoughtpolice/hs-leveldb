{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Exception.Lifted as Lifted
import Control.Exception

main :: IO ()
main = do
  dbname <- (("/tmp/leveldb_hs_test-"++) . show) `liftM` getEffectiveUserID
  say $ "creating db " ++ dbname
  let opts = def {dbCreateIfMissing = True, dbErrorIfExists = True}
  destroy dbname opts

  withDB dbname opts $ \db -> do
    say "stats"
    s <- property db DBStats
    say_ s

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

    say "compacting"
    compactAll db
    say_ "ok"

  say "repair"
  repair dbname def

  say "stats"
  withDB dbname def $ \db -> do
    say_ "number of files at levels:"
    Lifted.catch (do property db (NumFilesAtLevel 0) >>= say_
                     property db (NumFilesAtLevel 1) >>= say_
                     property db (NumFilesAtLevel 2) >>= say_
                     property db (NumFilesAtLevel 3) >>= say_
                     property db (NumFilesAtLevel 4) >>= say_
                     property db (NumFilesAtLevel 5) >>= say_
                     property db (NumFilesAtLevel 6) >>= say_
                     -- This will throw an exception to be caught
                     property db (NumFilesAtLevel 7) >>= say_
                 ) (\(_::SomeException) -> return ())

    putBS db def "hi" "omg"
    putBS db def "lol" "omg"
    getBS db def "lol" >>= say_ . show
    say_ "database stats:"
    property db DBStats >>= say_
    say_ "sstable stats:"
    property db SSTables >>= say_

  say "end"

say_ :: MonadIO m => String -> m ()
say_ = liftIO . Prelude.putStrLn

say :: MonadIO m => String -> m ()
say x = say_ $ "===== " ++ x ++ " ====="
