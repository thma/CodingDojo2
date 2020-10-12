module RpnCalculator where

import           Data.List       (isInfixOf)
import           Data.List.Split (splitOn)
-- this would be the default stack library:
--import           Data.Stack      (Stack, stackNew, stackPush, stackPop, stackIsEmpty)

-- let's roll our own stack implementation:
import           Stack           (Stack, stackIsEmpty, stackNew, stackPop,
                                  stackPush)
                                  
rpn :: String -> Integer
rpn input =
  let tokens = splitOn " " input
      maybeResult = stackPop (processTokens tokens stackNew)
  in  case maybeResult of
        Nothing     -> error "you did not provide enough arguments to rpn"
        Just (_s,x) -> x

processTokens :: [String] -> Stack Integer -> Stack Integer
processTokens [] stack = stack
processTokens (tok:tokens) stack
  | isUnaryOp tok =
      let maybePopped = stackPop stack
      in case maybePopped of
          Nothing -> error "empty stack in unary operation"
          Just (s', firstArg) -> processTokens tokens (stackPush s' (selectUnaryOp tok firstArg))
  | isBinOp tok =
      let maybePopped2 = pop2 stack
      in case maybePopped2 of
          Nothing -> error "empty stack in binary operation"
          Just (fstArg, secArg, s'') -> processTokens tokens (stackPush s'' (selectBinOp tok fstArg secArg))
  | isNaryOp tok =
      let args = toList stack
      in processTokens tokens (stackPush stackNew (selectNaryOp tok args))
  -- otherwise it must be an Integer
  | otherwise = processTokens tokens (stackPush stack (read tok))
  where
    pop2 :: Stack a -> Maybe (a, a, Stack a)
    pop2 s = do
      (s',  secArg) <- stackPop s
      (s'', fstArg) <- stackPop s'
      pure (fstArg, secArg, s'')

toList :: Stack a -> [a]
toList stack
 | stackIsEmpty stack = []
 | otherwise =
    let maybeS' = stackPop stack
    in  case maybeS' of
          Nothing         -> error "empty stack for toList"
          Just (s',  top) -> top : toList s'

isUnaryOp :: String -> Bool
isUnaryOp x = x `isInfixOf` "SQRT"

isBinOp :: String -> Bool
isBinOp x = x `isInfixOf` "+-*/"

isNaryOp :: String -> Bool
isNaryOp x = x `isInfixOf` "MAXMIN"

selectNaryOp :: String -> ([Integer] -> Integer)
selectNaryOp "MAX" = maximum
selectNaryOp "MIN" = minimum

selectUnaryOp :: String -> (Integer -> Integer)
selectUnaryOp "SQRT" = floor . sqrt . fromInteger

selectBinOp :: String -> (Integer -> Integer -> Integer)
selectBinOp "+" = (+)
selectBinOp "-" = (-)
selectBinOp "*" = (*)
selectBinOp "/" = div
