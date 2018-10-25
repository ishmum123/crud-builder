{-# LANGUAGE ScopedTypeVariables #-}

module Main where

main :: IO ()
main = do
  putStrLn "Please enter the length of the board: "
  lengthOfBoard :: Int <- readLn
  putStrLn $ "You have a board of size: " ++ show (lengthOfBoard * lengthOfBoard)