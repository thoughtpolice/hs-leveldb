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

import Data.Serialize hiding (get, put)

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Exception.Lifted as Lifted
import Control.Exception

import Control.Monad (when, forM_)

main :: IO ()
main = do
  dbname <- (("/tmp/leveldb_hs_test-"++) . show) `liftM` getEffectiveUserID
  say $ "creating db " ++ dbname
  let opts = def {dbCreateIfMissing = True, dbErrorIfExists = True}
  destroy dbname opts

  withDB dbname opts $ \db -> do
    say "begin bench"
    say_ "inserting 10,000,000 kv pairs... "
    forM_ [1..10::Int] $ \i -> do
      put db def ("abcdefghijkl"::String) ("omghilol" :: String)
    say_ "ok\n"
    printStats db
  withDB dbname def $ \db -> do
    {-
    say_ "retrieving 10,000,000 kv pairs..."
    forM_ [1..10::Int] $ \i -> do
      (_ :: String) <- get db def ("abcdefghijkl"++show i)
      return ()
    -}
    say_ "ok\n"
    say "end"
    printStats db

printStats db = do
  say "stats"
  property db DBStats >>= say_
  say_ "\n"
  property db SSTables >>= say_

say_ :: MonadIO m => String -> m ()
say_ = liftIO . Prelude.putStr

say :: MonadIO m => String -> m ()
say x = say_ $ "===== " ++ x ++ " =====\n"
