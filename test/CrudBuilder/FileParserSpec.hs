{-# LANGUAGE OverloadedStrings #-}

module CrudBuilder.FileParserSpec where

import Prelude hiding (max)
import System.Directory (getCurrentDirectory)
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

  describe "parseDatabase" $ do
    it "parses database json correctly" $ do
      dir <- getCurrentDirectory
      database <- parseFile (dir ++ "/test/resources/database.json")
      parseDatabase database
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
      evaluate (parseDatabase malformedData) `shouldThrow` errorCall "Invalid Database Structure"
