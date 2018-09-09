module TicTacToe.Logic (getLeftDiagonal, getRightDiagonal, transpose, allElemsEqual) where

-- TODO: Use point free
getLeftDiagonal :: [[a]] -> [a]
getLeftDiagonal = getDiagonal head tail

getRightDiagonal :: [[a]] -> [a]
getRightDiagonal = getDiagonal last init

getDiagonal :: ([a] -> a) -> ([a] -> [a]) -> [[a]] -> [a]
getDiagonal _ _ [] = []
getDiagonal f1 f2 matrix = (f1 $ head matrix) : getDiagonal f1 f2 (map f2 $ tail matrix)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = transpose []
transpose matrix = map head matrix : (transpose . map tail) matrix

allElemsEqual :: Eq a => [a] -> Bool
allElemsEqual [] = True
allElemsEqual (x : xs) = and $ map (== x) xs