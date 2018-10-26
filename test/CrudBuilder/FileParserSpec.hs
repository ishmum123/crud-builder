{-# LANGUAGE OverloadedStrings #-}

module CrudBuilder.FileParserSpec where

import Prelude hiding (max)
import System.Directory (getCurrentDirectory)
import Data.Text (pack)
import Test.Hspec (shouldBe, shouldReturn, shouldThrow, errorCall, Spec, it, describe)
import Control.Exception (evaluate)

import CrudBuilder.FileParser
import CrudBuilder.Model

spec :: Spec
spec = describe "FileParserSpec" $ do

  describe "parseFile" $ do
    it "parseFiles files correctly" $ do
      dir <- getCurrentDirectory
      parseFile (dir ++ "/test/resources/random.txt") `shouldReturn` "\"hello\""

  describe "parseDatabaseFromString" $ do
    it "parses database json correctly" $ do
      dir <- getCurrentDirectory
      database <- parseFile (dir ++ "/test/resources/database.json")
      parseDatabaseFromString database
        `shouldBe` Database {tables=[
                        Table {ttype="entity", tname="user", columns=[
                          Column {ctype="string", cname="name", constraints=Nothing, cdefault=Nothing, max=Nothing},
                           Column {ctype="string", cname="email", constraints=Nothing, cdefault=Nothing, max=Nothing}
                          ]},
                        Table {ttype="enum", tname="status", columns=[
                          Column {ctype="string", cname="name", constraints=Nothing, cdefault=Nothing, max=Just 10}
                          ]}
                       ],
                       relations=Just [Relation {referer="user.status", referenced="status"}]}

    it "throws exception when given malformed json" $ do
      dir <- getCurrentDirectory
      malformedData <- parseFile (dir ++ "/test/resources/database.json.malformed")
      evaluate (parseDatabaseFromString malformedData) `shouldThrow` errorCall "Invalid Database Structure"

  describe "parseColumnToLiquibaseString" $ do
    it "generates liquibase column data from a Table" $ do
      parseColumnToLiquibaseString Column {ctype="string", cname="email", constraints=Nothing, cdefault=Nothing, max=Nothing}
        `shouldBe` "\t\t\t\t\t\t\t\t\t- column:\n\
        \\t\t\t\t\t\t\t\t\t\t\tname: email\n\
        \\t\t\t\t\t\t\t\t\t\t\ttype: varchar\n"

  describe "generateConstraintDefinitionLiquibaseString" $ do
    it "generates liquibase description of constraint definitions" $ do
      generateConstraintDefinitionLiquibaseString (Just ["unique", "non-null"])
        `shouldBe` "\t\t\t\t\t\t\t\t\t\t\tconstraints:\n\
        \\t\t\t\t\t\t\t\t\t\t\t\tunique: true\n\
        \\t\t\t\t\t\t\t\t\t\t\t\tnullable: false\n"