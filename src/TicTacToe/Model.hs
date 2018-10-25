{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}

module TicTacToe.Model (Relation(..), Column(..), Table(..), Database(..)) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, Value(Object), parseJSON, (.:), (.:?))

data Relation = Relation
  {
    referer :: Text
  , referenced :: Text
  } deriving (Show, Generic, Eq, FromJSON)

-- type & default are haskell key-words
-- using ctype & cdefault instead
data Column = Column
  {
    ctype :: Text
  , cname :: Text
  , constraints :: Maybe [Text]
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

-- duplicate declarations of name
-- using cname & tname instead
data Table = Table
  {
    ttype :: Text
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
