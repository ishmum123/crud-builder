module TicTacToe.Model (Move(..), Position(..), Board(..), moveAtPosition, positionOfBoard, getBoardOfLength) where

data Move = X | Y deriving (Eq, Show)

data Position = Position (Maybe Move) deriving (Eq, Show)

data Board = Board [[Position]] deriving (Eq, Show)

moveAtPosition :: Position -> Maybe Move
moveAtPosition (Position x) = x

positionOfBoard :: Board -> Int -> Int -> Position
positionOfBoard (Board board) row column = board !! row !! column

getBoardOfLength :: Int -> Board
getBoardOfLength 0 = Board []
getBoardOfLength len = Board . replicate len . replicate len . Position $ Nothing