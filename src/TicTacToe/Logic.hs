module TicTacToe.Logic (leftDiagonal, rightDiagonal, leftDiagonals, rightDiagonals, transpose, elemsEqual) where

-- TODO: Use point free
leftDiagonal :: [[a]] -> [a]
leftDiagonal = getDiagonal head tail

leftDiagonals :: [[a]] -> [[a]]
leftDiagonals [] = []
leftDiagonals matrix = leftDiagonalsFromAndBelowPrimary matrix
  ++ (leftDiagonalsFromAndBelowPrimary $ tail $ transpose matrix)

leftDiagonalsFromAndBelowPrimary :: [[a]] -> [[a]]
leftDiagonalsFromAndBelowPrimary [] = []
leftDiagonalsFromAndBelowPrimary matrix = leftDiagonal matrix : (leftDiagonalsFromAndBelowPrimary $ tail matrix)

rightDiagonal :: [[a]] -> [a]
rightDiagonal = getDiagonal last init

rightDiagonals :: [[a]] -> [[a]]
rightDiagonals [] = []
rightDiagonals matrix = rightDiagonalsFromAndAbovePrimary matrix ++ (rightDiagonalsFromAndBelowPrimary $ tail matrix)

rightDiagonalsFromAndAbovePrimary :: [[a]] -> [[a]]
rightDiagonalsFromAndAbovePrimary ([] : _) = []
rightDiagonalsFromAndAbovePrimary matrix = rightDiagonal matrix : (rightDiagonalsFromAndAbovePrimary $ map init matrix)

rightDiagonalsFromAndBelowPrimary :: [[a]] -> [[a]]
rightDiagonalsFromAndBelowPrimary [] = []
rightDiagonalsFromAndBelowPrimary matrix = rightDiagonal matrix : (rightDiagonalsFromAndBelowPrimary $ tail matrix)

getDiagonal :: ([a] -> a) -> ([a] -> [a]) -> [[a]] -> [a]
getDiagonal _ _ [] = []
getDiagonal _ _ ([] : _) = []
getDiagonal singleMember subList matrix = (singleMember $ head matrix)
  : getDiagonal singleMember subList (map subList $ tail matrix)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = transpose []
transpose matrix = map head matrix : (transpose . map tail) matrix

elemsEqual :: Eq a => [a] -> Bool
elemsEqual [] = True
elemsEqual (x : xs) = and $ map (== x) xs