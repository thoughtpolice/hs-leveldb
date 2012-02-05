-- |
-- Module      : Database.LevelDB.FFI
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides low-level FFI bindings to the 'LevelDB' C API.
-- 
-- This binding is based on LevelDB git revision \'239ac9d2dea0ac1708b7d903a3d80d3883e0781b\'.
-- 
module Database.LevelDB
       (
       ) where

#include <leveldb/c.h>

