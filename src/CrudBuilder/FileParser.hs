{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}

module CrudBuilder.FileParser where

import Data.Aeson (decode)
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as B

import CrudBuilder.Model

parseFile :: String -> IO B.ByteString
parseFile = B.readFile

parseDatabaseFromString :: B.ByteString -> Database
parseDatabaseFromString contents =
  case (decode contents :: Maybe Database) of
    Nothing -> error "Invalid Database Structure"
    Just db -> db

parseColumnToLiquibaseString :: Column -> String
parseColumnToLiquibaseString (Column t n c _ _) = "\t\t\t\t\t\t\t\t\t- column:\n"
  ++ "\t\t\t\t\t\t\t\t\t\t\tname: " ++ unpack n ++ "\n"
  ++ "\t\t\t\t\t\t\t\t\t\t\ttype: varchar\n" ++ generateConstraintDefinitionLiquibaseString c

generateConstraintDefinitionLiquibaseString :: Maybe [Text] -> String
generateConstraintDefinitionLiquibaseString constraints =
  case constraints of
    Nothing -> ""
    Just constraint -> if null constraints then ""; else "\t\t\t\t\t\t\t\t\t\t\tconstraints:\n\
              \\t\t\t\t\t\t\t\t\t\t\t\tunique: true\n\
              \\t\t\t\t\t\t\t\t\t\t\t\tnullable: false\n"