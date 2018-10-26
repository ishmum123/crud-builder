{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}

module CrudBuilder.FileParser (parseFile, parseDatabase) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B

import CrudBuilder.Model (Database)

parseFile :: String -> IO B.ByteString
parseFile = B.readFile

parseDatabase :: B.ByteString -> Database
parseDatabase contents =
  case (decode contents :: Maybe Database) of
    Nothing -> error "Invalid Database Structure"
    Just db -> db
