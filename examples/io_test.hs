{-# LANGUAGE OverloadedStrings #-}
module Main
       ( main -- :: IO ()
       ) where

import Data.ByteString.Char8 as S
import Control.Monad (liftM)
import System.Posix.User (getEffectiveUserID)
import Database.LevelDB.IO as LDB
import System.Exit

main :: IO ()
main = do
  dbname <- (("/tmp/leveldb_hs_test-"++) . show) `liftM` getEffectiveUserID
  say $ "creating db " ++ dbname

  let opts = defaultDBOptions{dbCreateIfMissing = True, dbErrorIfExists = True}
  LDB.destroy opts dbname
  db' <- LDB.open opts dbname
  db <- case db' of
  	Left e  -> Prelude.putStrLn ("err: " ++ e) >> exitWith (ExitFailure 1)
  	Right r -> do
  	  say "stats"
	  p <- LDB.property r "leveldb.stats"
  	  case p of
  	  	Just x -> Prelude.putStrLn x
  	  	_      -> return ()
  	  return r

  say "put"
  LDB.put db defaultWriteOptions "foo" "hello"
  LDB.put db defaultWriteOptions "box" "c"

  say "get"
  Right x <- LDB.get db defaultReadOptions "foo"
  S.putStrLn x
  Right x <- LDB.get db defaultReadOptions "box"
  S.putStrLn x

  say "snapshot"
  snap <- LDB.createSnapshot db
  LDB.delete db defaultWriteOptions "foo"
  Right x <- LDB.get db defaultReadOptions{readSnapshot = Just snap} "foo"
  S.putStrLn x
  Right x <- LDB.get db defaultReadOptions "foo"
  S.putStrLn x
  releaseSnapshot snap

  say "repair"
  LDB.close db
  let opts = defaultDBOptions
  Nothing <- LDB.repair opts dbname
  db' <- LDB.open opts dbname
  db <- case db' of
  	Left e  -> Prelude.putStrLn ("err: " ++ e) >> exitWith (ExitFailure 1)
  	Right r -> do
  	  say "stats"
	  p <- LDB.property r "leveldb.stats"
  	  case p of
  	  	Just x -> Prelude.putStrLn x
  	  	_      -> return ()
  	  return r
  Right x <- LDB.get db defaultReadOptions "box"
  S.putStrLn x
  say "cleanup"
  LDB.close db

say :: String -> IO ()
say x = Prelude.putStrLn $ "===== " ++ x ++ " ====="
