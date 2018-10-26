{-# LANGUAGE OverloadedStrings #-}

module CrudBuilder.ModelSpec where

import Test.Hspec (shouldBe, shouldNotBe, Spec, it, describe)
import Data.Aeson (decode, encode)
import Prelude hiding (max)

import CrudBuilder.Model

spec :: Spec
spec = describe "Model" $ do

  describe "Relation" $ do
    it "creates relations correctly" $ do
      (decode "{\"referer\":\"user.status\",\"referenced\": \"status\"}" :: Maybe Relation)
      `shouldBe` Just Relation {referer="user.status", referenced="status"}

    it "creates relations correctly with unordered data" $ do
      (decode "{\"referenced\": \"status\", \"referer\":\"user.status\"}" :: Maybe Relation)
      `shouldBe` Just Relation {referer="user.status", referenced="status"}

    it "does not parse ill-formed json (colon missing)" $ do
      (decode "{\"referer\":\"user.status\",\"referenced\" \"status\"}" :: Maybe Relation)
      `shouldBe` Nothing

    it "does not parse data-missing json" $ do
      (decode "{\"referenced\": \"status\"}" :: Maybe Relation)
      `shouldBe` Nothing

  describe "Column" $ do
    it "creates columns correctly" $ do
      (decode "{\"type\":\"string\",\"name\": \"name\",\"constraints\": [\"unique\", \"non-null\"], \
        \\"default\": \"hello\", \"max\": 4}" :: Maybe Column)
      `shouldBe` Just Column {ctype="string", cname="name", constraints=Just ["unique", "non-null"],
        cdefault=Just "hello", max=Just 4}

    it "creates columns correctly with missing non-required data" $ do
      (decode "{\"type\":\"string\",\"name\": \"name\"}" :: Maybe Column)
      `shouldBe` Just Column {ctype="string", cname="name", constraints=Nothing, cdefault=Nothing, max=Nothing}

  describe "Table" $ do
    it "creates tables correctly with multiple columns" $ do
      (decode "{\"type\":\"entity\",\"name\": \"user\",\
        \\"columns\": [{\"type\":\"string\",\"name\": \"name\"}, \
        \{\"type\":\"string\",\"name\": \"email\"}]}" :: Maybe Table)
      `shouldBe` Just Table {ttype="entity", tname="user",
        columns=[Column {ctype="string", cname="name", constraints=Nothing, cdefault=Nothing, max=Nothing},
                  Column {ctype="string", cname="email", constraints=Nothing, cdefault=Nothing, max=Nothing}]}

    it "does not create table with ill-defined column description (column type missing)" $ do
      (decode "{\"type\":\"entity\",\"name\": \"user\",\
        \\"columns\": [{\"name\": \"name\"}, {\"name\": \"email\"}]}" :: Maybe Table)
      `shouldBe` Nothing

    -- TODO: Should not create table with no columns
    {-it "creates tables correctly with no columns" $ do
      (decode "{\"type\":\"entity\",\"name\": \"user\",\"columns\": []}" :: Maybe Table)
      `shouldBe` Nothing-}

  describe "Database" $ do
    it "creates database correctly" $ do
      (decode "{\"tables\": [{\"type\":\"entity\",\"name\": \"user\",\
         \\"columns\": [{\"type\":\"string\",\"name\": \"name\"}, \
           \{\"type\":\"string\",\"name\": \"email\"}]}]}" :: Maybe Database)
      `shouldBe` Just Database {tables=[Table {ttype="entity", tname="user",
                  columns=[Column {ctype="string", cname="name", constraints=Nothing, cdefault=Nothing, max=Nothing},
                    Column {ctype="string", cname="email", constraints=Nothing, cdefault=Nothing, max=Nothing}]}],
                  relations=Nothing}

    -- TODO: Should not create database with no tables
    {-it "creates tables correctly with no columns" $ do
      (decode "{\"tables\": []}" :: Maybe Database)
      `shouldBe` Nothing-}