module GuardScratch where

import           Control.Monad
import           Data.List

-- just playing with guards to see
-- them as there isn't enough
-- examples in the book

g1 :: Int -> [Int]
g1 i1 = do
  i1' <- [1 .. i1]
  guard $ i1' >= 5 -- aka filter
  return i1'

-- g1 10
-- [5, 6, 7, 8, 9, 10]

g2 :: Int -> [Int]
g2 i1 = do
  i1' <- [1 .. i1]
  guard $ i1' >= 5
  let i2 = i1' * 100
  guard $ i2 <= 700
  return i2


generateNextLk xs = do
  a <- xs
  b <- xs
  return (a, b)
