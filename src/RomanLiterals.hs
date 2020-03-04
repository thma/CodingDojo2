module RomanLiterals where

import Data.Char (toLower)

singleChartable :: [(Char, Integer)]
singleChartable = [('i',1),('v',5),('x',10),('l',50),('c',100),('d',500),('m',1000)]

doubleCharTable :: [(String,Integer)]
doubleCharTable = [("iv",4),("ix",9),("xl",40),("xc",90),("cd",400), ("cm",900)]

convert :: String -> Integer
convert str = convert2 (strToLower str)
  where strToLower = map toLower

convert2 :: String -> Integer
convert2 (c1:c2:rest) =
  case lookup [c1, c2] doubleCharTable of
    Nothing  -> convert2 [c1] + convert2 (c2 : rest)
    Just val -> val + convert2 rest
convert2 (char : rest) =
  case lookup char singleChartable of
    Nothing  -> error "should not happen"
    Just val -> val + convert2 rest
convert2 []           = 0

