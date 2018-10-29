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

instance FromJSON ColumnType where
    parseJSON (String "string") = return CString
    parseJSON (String "date") = return CDate
    parseJSON (String "bool") = return CBool
    parseJSON (String "num") = return CNum
    parseJSON (String "clob") = return CClob
    parseJSON (String "uuid") = return CUuid
    parseJSON _ = mzero

data Constraint = Unique | Required
  deriving (Show, Generic, Eq)

instance FromJSON Constraint where
    parseJSON (String "unique") = return Unique
    parseJSON (String "required") = return Required
    parseJSON _ = mzero

data Column = Column
  {
    ctype :: ColumnType
  , cname :: Text
  , constraints :: Maybe [Constraint]
  , cdefault :: Maybe Text
  , max :: Maybe Int
  } deriving (Show, Generic, Eq)

instance FromJSON Column where
 parseJSON (Object v) =
    Column <$> v .: "type"
           <*> v .: "name"
           <*> v .:? "constraints"
           <*> v .:? "default"
           <*> v .:? "max"

data TableType = Entity | Enum | Mapping
  deriving (Show, Generic, Eq)

instance FromJSON TableType where
    parseJSON (String "entity") = return Entity
    parseJSON (String "enum") = return Enum
    parseJSON (String "mapping") = return Mapping
    parseJSON _ = mzero

data Table = Table
  {
    ttype :: TableType
  , tname :: Text
  , columns :: [Column]
  } deriving (Show, Generic, Eq)

instance FromJSON Table where
 parseJSON (Object v) =
    Table <$> v .: "type"
           <*> v .: "name"
           <*> v .: "columns"

data Database = Database
  {
    tables :: [Table]
  , relations :: Maybe [Relation]
  } deriving (Show, Generic, Eq, FromJSON)
