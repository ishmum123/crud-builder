module TicTacToe.ModelSpec where

import Test.Hspec (shouldBe, shouldNotBe, Spec, it, describe)

import TicTacToe.Model (Move(X, Y), Position(..), Board(..), moveAtPosition, positionOfBoard)

spec :: Spec
spec = describe "Model" $ do

  describe "Move" $ do
    it "is equal if symbols are equal" $ do
      X `shouldBe` X

    it "is not equal if symbols are different" $ do
      X `shouldNotBe` Y

  describe "moveAtPosition" $ do
    it "returns a move if available" $ do
      moveAtPosition (Position $ Just X) `shouldBe` Just X

    it "returns nothing if a move is not available" $ do
      moveAtPosition (Position $ Nothing) `shouldBe` Nothing

  -- TODO: Use type safe way to access board
  describe "positionOfBoard" $ do
    it "returns the respective position at board" $ do
       positionOfBoard (Board ([[Position $ Nothing]])) 0 0 `shouldBe` (Position $ Nothing)