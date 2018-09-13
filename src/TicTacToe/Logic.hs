module TicTacToe.Logic (leftDiagonal, rightDiagonal, leftDiagonals, rightDiagonals, transpose, combinations, validCombinations, elemsEqual) where

-- TODO: Use point free
leftDiagonal :: [[a]] -> [a]
leftDiagonal = diagonal head tail

leftDiagonals :: [[a]] -> [[a]]
leftDiagonals [] = []
leftDiagonals matrix = leftDiagonalsFromAndBelowPrimary matrix
  ++ (leftDiagonalsFromAndBelowPrimary . tail . transpose $ matrix)

leftDiagonalsFromAndBelowPrimary :: [[a]] -> [[a]]
leftDiagonalsFromAndBelowPrimary [] = []
leftDiagonalsFromAndBelowPrimary matrix = leftDiagonal matrix : (leftDiagonalsFromAndBelowPrimary . tail $ matrix)

rightDiagonal :: [[a]] -> [a]
rightDiagonal = diagonal last init

rightDiagonals :: [[a]] -> [[a]]
rightDiagonals [] = []
rightDiagonals matrix = rightDiagonalsFromAndAbovePrimary matrix ++ (rightDiagonalsFromAndBelowPrimary . tail $ matrix)

rightDiagonalsFromAndAbovePrimary :: [[a]] -> [[a]]
rightDiagonalsFromAndAbovePrimary ([] : _) = []
rightDiagonalsFromAndAbovePrimary matrix = rightDiagonal matrix : (rightDiagonalsFromAndAbovePrimary . map init $ matrix)

rightDiagonalsFromAndBelowPrimary :: [[a]] -> [[a]]
rightDiagonalsFromAndBelowPrimary [] = []
rightDiagonalsFromAndBelowPrimary matrix = rightDiagonal matrix : (rightDiagonalsFromAndBelowPrimary . tail $ matrix)

diagonal :: ([a] -> a) -> ([a] -> [a]) -> [[a]] -> [a]
diagonal _ _ [] = []
diagonal _ _ ([] : _) = []
diagonal singleMember subList matrix = (singleMember . head $ matrix)
  : diagonal singleMember subList (map subList . tail $ matrix)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = transpose []
transpose matrix = map head matrix : (transpose . map tail) matrix

baseTransformations :: [([[a]] -> [[a]])]
baseTransformations = [id, transpose]

combinations :: [[a]] -> [[a]]
combinations = concatTransformations $ baseTransformations ++ [leftDiagonals, rightDiagonals]

validCombinations :: [[a]] -> [[a]]
validCombinations = concatTransformations $ baseTransformations ++ [biggestDiagonals . diagonals]

biggestDiagonals :: [[a]] -> [[a]]
biggestDiagonals matrix = filter (\x -> length x == maxLength matrix) matrix
  where maxLength = maximum . map length

diagonals :: [[a]] -> [[a]]
diagonals = concatTransformations [leftDiagonals, rightDiagonals]

concatTransformations :: [([[a]] -> [[a]])] -> [[a]] -> [[a]]
concatTransformations transformations matrix = foldl1 (++) $ map ($ matrix) transformations

elemsEqual :: Eq a => [a] -> Bool
elemsEqual [] = True
elemsEqual (x : xs) = and $ map (== x) xs