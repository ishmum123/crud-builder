{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}

module CrudBuilder.FileParser where

import Data.Aeson (decode)
import Data.Text (Text, unpack)
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as B

import CrudBuilder.Model

tabChar :: String
tabChar = "    "

getNTabs :: Int -> String
getNTabs n = foldl (++) "" $ replicate n tabChar

columnTagTabs :: String
columnTagTabs = getNTabs 9

columnDefinitionTabs :: String
columnDefinitionTabs = getNTabs 11

constraintDefinitionTabs :: String
constraintDefinitionTabs = getNTabs 12

parseFile :: String -> IO B.ByteString
parseFile = B.readFile

writeToFile :: String -> String -> IO ()
writeToFile path text = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path text

parseDatabaseFromString :: B.ByteString -> Database
parseDatabaseFromString contents =
  case (decode contents :: Maybe Database) of
    Nothing -> error "Invalid Database Structure"
    Just db -> db

parseColumnToLiquibaseString :: Column -> String
parseColumnToLiquibaseString (Column t n c _ _) =  columnTagTabs ++ "- column:\n"
  ++ columnDefinitionTabs ++ "name: " ++ unpack n ++ "\n"
  ++ columnDefinitionTabs ++ "type: varchar\n" ++ generateConstraintDefinitionLiquibaseString c

generateConstraintDefinitionLiquibaseString :: Maybe [Constraint] -> String
generateConstraintDefinitionLiquibaseString constraints =
  case constraints of
    Nothing -> ""
    Just constraint -> if null constraints then ""; else columnDefinitionTabs ++ "constraints:\n"
              ++ constraintDefinitionTabs ++ "unique: true\n"
              ++ constraintDefinitionTabs ++ "nullable: false\n"