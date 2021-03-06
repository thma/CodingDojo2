module TestUtils where

import           Test.Hspec            hiding (it)
import qualified Test.Hspec as HS      (it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

-- redefine `it` to use a sample with 1000 elements
it :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
it label action = modifyMaxSuccess (const 1000) $ HS.it label action

