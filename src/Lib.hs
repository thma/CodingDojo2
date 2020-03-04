module Lib
    ( someFunc
    , squared
    , manhattanDistance
    , add
    , Point (..)
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

squared :: (Num a) => a -> a
squared n = n*n

add a b = a + b

data Point = Point Int Int

manhattanDistance :: (Point, Point) -> Int
manhattanDistance (Point x y, Point x1 y1) = abs(x - x1) + abs(y - y1)

