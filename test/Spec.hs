import Test.Hspec (hspec, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Data.Stack (StackM, pop, push, runStack)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "add3" $ do
    prop "add3" $ \i j k -> runStack (add3 i j k) == Just (i + j + k)
  describe "Pop empty stack" $ do
    it "return Nothing" $ do
      runStack (push (1 :: Int) >> pop >> pop) `shouldBe` Nothing

addTop2 :: Num s => StackM s ()
addTop2 = do
  i <- pop
  j <- pop
  push $ i + j

add3 :: Int -> Int -> Int -> StackM Int Int
add3 i j k = do
  push i
  push j
  addTop2
  push k
  addTop2
  pop
