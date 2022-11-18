module Stack (
   Stack,
   stackNew,
   stackPush,
   stackPeek,
   stackPop,
   stackIsEmpty,
   stackReverse
 ) where

import Data.Map ()

newtype Stack a = Stck [a] deriving (Show, Read, Eq, Ord)

stackNew :: Stack a
stackNew = Stck []

stackPop :: Stack a -> Maybe (Stack a, a)
stackPop (Stck [])     = Nothing
stackPop (Stck (x:xs)) = Just (Stck xs, x)

stackPush :: Stack a -> a -> Stack a
stackPush (Stck l) x = Stck (x : l)

stackReverse :: Stack a -> Stack a
stackReverse stack = stackRev stack stackNew 
  where 
    stackRev :: Stack a -> Stack a -> Stack a
    stackRev (Stck [])     acc = acc
    stackRev (Stck (x:xs)) acc = stackRev (Stck xs) (stackPush acc x)

stackIsEmpty :: Stack a -> Bool
stackIsEmpty (Stck []) = True
stackIsEmpty _         = False

stackPeek :: Stack a -> Maybe a
stackPeek (Stck [])     = Nothing
stackPeek (Stck (x:_))  = Just x