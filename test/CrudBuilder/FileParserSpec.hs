{-# LANGUAGE OverloadedStrings #-}

module CrudBuilder.FileParserSpec where

import Prelude hiding (max)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (shouldBe, shouldReturn, shouldThrow, errorCall, Spec, it, describe)
import Control.Exception (evaluate)

import CrudBuilder.FileParser
import CrudBuilder.Model

spec :: Spec
spec = describe "FileParser" $ do

  describe "parseFile" $ do
    it "parses files correctly" $ do
      dir <- getCurrentDirectory
      parseFile (dir </> "test" </> "resources" </> "random.txt") `shouldReturn` "        \"hello\"\n"

  describe "writeToFile" $ do
    it "writes to files correctly" $ do
      dir <- getCurrentDirectory
      let testFile = dir </> "test" </> "out" </> "test.txt"
      writeToFile testFile "        \"hello\"\n"
      parseFile testFile `shouldReturn` "        \"hello\"\n"

  describe "parseDatabaseFromString" $ do
    it "parses database json correctly" $ do
      dir <- getCurrentDirectory
      database <- parseFile (dir </> "test" </> "resources" </> "database.json")
      parseDatabaseFromString database
        `shouldBe` Database {tables=[
                        Table {ttype=Entity, tname="user", columns=[
                          Column {ctype=CString, cname="name", constraints=Nothing, cdefault=Nothing, max=Nothing},
                           Column {ctype=CString, cname="email", constraints=Nothing, cdefault=Nothing, max=Nothing}
                          ]},
                        Table {ttype=Enum, tname="status", columns=[
                          Column {ctype=CString, cname="name", constraints=Nothing, cdefault=Nothing, max=Just 10}
                          ]}
                       ],
                       relations=Just [Relation {referer="user.status", referenced="status"}]}

    it "throws exception when given malformed json" $ do
      dir <- getCurrentDirectory
      malformedData <- parseFile (dir </> "test" </> "resources" </> "database.json.malformed")
      evaluate (parseDatabaseFromString malformedData) `shouldThrow` errorCall "Invalid Database Structure"

  describe "parseColumnToLiquibaseString" $ do
    it "generates liquibase column data from a Table" $ do
      parseColumnToLiquibaseString Column {ctype=CString, cname="email", constraints=Nothing, cdefault=Nothing, max=Nothing}
        `shouldBe` "                                    - column:\n\
        \                                            name: email\n\
        \                                            type: varchar\n"

  describe "generateConstraintDefinitionLiquibaseString" $ do
    it "generates liquibase description of constraint definitions" $ do
      generateConstraintDefinitionLiquibaseString (Just [Unique, Required])
        `shouldBe` "                                            constraints:\n\
        \                                                unique: true\n\
        \                                                nullable: false\n"