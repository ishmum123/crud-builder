{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}

module CrudBuilder.Model (
  Relation(..),
  Column(..),
  Table(..),
  Database(..),
  TableType(..),
  ColumnType(..),
  Constraint(..) ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Monad (mzero)
import Data.HashMap.Strict (keys)
import Data.Aeson (FromJSON, Value(String, Object), parseJSON, (.:), (.:?))

data Relation = Relation
  {
    referer :: Text
  , referenced :: Text
  } deriving (Show, Generic, Eq, FromJSON)

data ColumnType = CString | CDate | CBool | CNum | CClob | CUuid
  deriving (Show, Generic, Eq)

data Constraint = Unique | Required
  deriving (Show, Generic, Eq)

data Column = Column
  {
    ctype :: ColumnType
  , cname :: Text
  , constraints :: Maybe [Constraint]
  , cdefault :: Maybe Text
  , max :: Maybe Int
  } deriving (Show, Generic, Eq)

data TableType = Entity | Enum | Mapping
  deriving (Show, Generic, Eq)

data Table = Table
  {
    ttype :: TableType
  , tname :: Text
  , columns :: [Column]
  } deriving (Show, Generic, Eq)

data Database = Database
  {
    tables :: [Table]
  , relations :: Maybe [Relation]
  } deriving (Show, Generic, Eq, FromJSON)



-----------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------WARNING!!!------------------------------------------------------
--------------------------------------------------- UGLY CODE BELOW ---------------------------------------------------
------------------------------------------ MOVE TO SEPARATE FILE IF POSSIBLE ------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

instance FromJSON ColumnType where
    parseJSON (String s)
        | s == "string" = return CString
        | s == "date" = return CDate
        | s == "bool" = return CBool
        | s == "num" = return CNum
        | s == "clob" = return CClob
        | s == "uuid" = return CUuid
        | otherwise = mzero

instance FromJSON Constraint where
    parseJSON (String s)
        | s == "unique" = return Unique
        | s == "required" = return Required
        | otherwise = mzero

instance FromJSON Column where
 parseJSON (Object v) =
    Column <$> v .: "type"
           <*> v .: "name"
           <*> v .:? "constraints"
           <*> v .:? "default"
           <*> v .:? "max"

instance FromJSON TableType where
    parseJSON (String s)
        | s == "entity" = return Entity
        | s == "enum" = return Enum
        | s == "mapping" = return Mapping
        | otherwise = mzero

instance FromJSON Table where
 parseJSON (Object v) =
    Table <$> v .: "type"
           <*> v .: "name"
           <*> v .: "columns"