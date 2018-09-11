module TicTacToe.Logic (leftDiagonal, rightDiagonal, transpose, elemsEqual) where

-- TODO: Use point free
leftDiagonal :: [[a]] -> [a]
leftDiagonal = getDiagonal head tail

rightDiagonal :: [[a]] -> [a]
rightDiagonal = getDiagonal last init

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