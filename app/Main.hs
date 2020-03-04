module Main where

import Lib

main :: IO ()
main = do
  let n = 12
  putStrLn $ "The square of " ++ show n ++ " is " ++ show (squared n)

