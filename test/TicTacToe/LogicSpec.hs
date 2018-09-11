module TicTacToe.LogicSpec where

import Test.Hspec (shouldBe, shouldSatisfy, shouldNotSatisfy, Spec, it, describe)

import TicTacToe.Logic (leftDiagonal, rightDiagonal, transpose, elemsEqual)

-- TODO: Use QuickCheck for randomised testing
spec :: Spec
spec = do

  describe "Logic" $ do

    describe "leftDiagonal" $ do
      it "returns the left to right diagonal of 2x2 matrix" $ do
        leftDiagonal [[1, 2], [3, 4]] `shouldBe` [1, 4]

      it "returns the left to right diagonal of 1x2 matrix" $ do
        leftDiagonal [[1], [3]] `shouldBe` [1]

      it "returns the left to right diagonal of 2x1 matrix" $ do
        leftDiagonal [[1, 3]] `shouldBe` [1]

      it "returns the left to right diagonal of 4x4 matrix" $ do
        leftDiagonal [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 17, 98]] `shouldBe` [1, 6, 11, 98]

      it "returns the primary left to right diagonal of 3x2 matrix" $ do
        leftDiagonal [[1, 2, 3], [3, 4, 6]] `shouldBe` [1, 4]

      it "returns empty list for an empty list" $ do
        leftDiagonal ([] :: [[Int]]) `shouldBe` ([] :: [Int])

    describe "rightDiagonal" $ do
      it "returns the right to left diagonal of 2x2 matrix" $ do
        rightDiagonal [[1, 2], [3, 4]] `shouldBe` [2, 3]

      it "returns the right to left diagonal of 1x2 matrix" $ do
        rightDiagonal [[1], [3]] `shouldBe` [1]

      it "returns the right to left diagonal of 2x1 matrix" $ do
        rightDiagonal [[1, 3]] `shouldBe` [3]

      it "returns the right to left diagonal of 4x4 matrix" $ do
        rightDiagonal [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 17, 98]] `shouldBe` [4, 7, 10, 13]

      it "returns the primary right to left diagonal of 3x2 matrix" $ do
        rightDiagonal [[1, 2, 3], [3, 4, 6]] `shouldBe` [3, 4]

      it "returns empty list for an empty list" $ do
        rightDiagonal ([] :: [[Int]]) `shouldBe` ([] :: [Int])

    describe "transpose" $ do
      it "returns the columns of 2x2 matrix" $ do
        transpose [[1, 2], [3, 4]] `shouldBe` [[1, 3], [2, 4]]

      it "returns the columns of 4x4 matrix" $ do
        transpose [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 17, 98]]
        `shouldBe` [[1, 5, 9, 13], [2, 6, 10, 14], [3, 7, 11, 17], [4, 8, 12, 98]]

      it "returns the primary columns of 3x2 matrix" $ do
        transpose [[1, 2, 3], [3, 4, 6]] `shouldBe` [[1, 3], [2, 4], [3, 6]]

      it "returns empty for empty list" $ do
        transpose ([] :: [[Int]]) `shouldBe` ([] :: [[Int]])

    describe "elemsEqual" $ do
      it "returns true for 5 equal elems" $ do
        [7, 7, 7, 7, 7] `shouldSatisfy` elemsEqual

      it "returns true for 3 equal elems & 1 un-equal elem" $ do
        [4, 4, 4, 3, 4] `shouldNotSatisfy` elemsEqual

      it "returns true for 1 elem" $ do
        [4] `shouldSatisfy` elemsEqual

      it "returns true for empty list" $ do
        ([] :: [Int]) `shouldSatisfy` elemsEqual