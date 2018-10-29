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
      (decode "{\"referer\":\"user.status_id\",\"referenced\": \"status\"}" :: Maybe Relation)
        `shouldBe` Just Relation {referer="user.status_id", referenced="status"}

    it "creates relations correctly with unordered data" $ do
      (decode "{\"referenced\": \"status\", \"referer\":\"user.status_id\"}" :: Maybe Relation)
        `shouldBe` Just Relation {referer="user.status_id", referenced="status"}

    it "does not parse ill-formed json (colon missing)" $ do
      (decode "{\"referer\":\"user.status_id\",\"referenced\" \"status\"}" :: Maybe Relation)
        `shouldBe` Nothing

    it "does not parse data-missing json" $ do
      (decode "{\"referenced\": \"status\"}" :: Maybe Relation)
        `shouldBe` Nothing

  describe "Column" $ do
    it "creates columns correctly" $ do
      (decode "{\"type\":\"string\",\"name\": \"name\",\"constraints\": [\"unique\", \"required\"], \
        \\"default\": \"hello\", \"max\": 4}" :: Maybe Column)
        `shouldBe` Just Column {ctype=CString, cname="name", constraints=Just [Unique, Required],
        cdefault=Just "hello", max=Just 4}

    it "creates columns correctly with missing non-required data" $ do
      (decode "{\"type\":\"string\",\"name\": \"name\"}" :: Maybe Column)
        `shouldBe` Just Column {ctype=CString, cname="name", constraints=Nothing, cdefault=Nothing, max=Nothing}

  describe "Table" $ do
    it "creates tables correctly with multiple columns" $ do
      (decode "{\"type\":\"entity\",\"name\": \"user\",\
        \\"columns\": [{\"type\":\"string\",\"name\": \"name\"}, \
        \{\"type\":\"string\",\"name\": \"email\"}]}" :: Maybe Table)
        `shouldBe` Just Table {ttype=Entity, tname="user",
        columns=[Column {ctype=CString, cname="name", constraints=Nothing, cdefault=Nothing, max=Nothing},
                  Column {ctype=CString, cname="email", constraints=Nothing, cdefault=Nothing, max=Nothing}]}

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
        `shouldBe` Just Database {tables=[Table {ttype=Entity, tname="user",
                  columns=[Column {ctype=CString, cname="name", constraints=Nothing, cdefault=Nothing, max=Nothing},
                    Column {ctype=CString, cname="email", constraints=Nothing, cdefault=Nothing, max=Nothing}]}],
                  relations=Nothing}

    -- TODO: Should not create database with no tables
    {-it "creates tables correctly with no columns" $ do
      (decode "{\"tables\": []}" :: Maybe Database)
      `shouldBe` Nothing-}

  describe "TableType" $ do
    it "creates table types correctly" $ do
      (decode "[\"entity\",\"enum\",\"mapping\"]" :: Maybe [TableType]) `shouldBe` Just [Entity, Enum, Mapping]

    it "rejects undefined table types" $ do
      (decode "[\"undefined-table-type\"]" :: Maybe [TableType]) `shouldBe` Nothing

  describe "ColumnType" $ do
    it "creates column types correctly" $ do
      (decode "[\"string\",\"date\",\"clob\",\"num\",\"bool\",\"uuid\"]" :: Maybe [ColumnType])
        `shouldBe` Just [CString, CDate, CClob, CNum, CBool, CUuid]

    it "rejects undefined column types" $ do
      (decode "[\"undefined-column-type\"]" :: Maybe [ColumnType]) `shouldBe` Nothing

  describe "Constraint" $ do
    it "creates constraints correctly" $ do
      (decode "[\"unique\",\"required\"]" :: Maybe [Constraint])
        `shouldBe` Just [Unique, Required]

    it "rejects undefined constraints" $ do
      (decode "[\"undefined-column-type\"]" :: Maybe [Constraint]) `shouldBe` Nothing