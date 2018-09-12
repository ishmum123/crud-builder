module TicTacToe.Model (Move(..), Position(..), Board(..), moveAtPosition, positionOfBoard) where

data Move = X | O deriving (Eq, Show)

data Position = Position (Maybe Move) deriving (Eq, Show)

data Board = Board [[Position]]

moveAtPosition :: Position -> Maybe Move
moveAtPosition (Position x) = x

positionOfBoard :: Board -> Int -> Int -> Position
positionOfBoard (Board board) row column = board !! row !! column