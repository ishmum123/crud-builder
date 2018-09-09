module Lib
    ( someFunc
    ) where

import Control.Concurrent.Chan as Conc

h = Conc.dupChan

someFunc :: IO ()
someFunc = print (g 1)

newtype Id a = Id a

instance Functor Id where
  fmap f (Id a) = Id (f a)

g :: Int -> Int
g = (+ 1)