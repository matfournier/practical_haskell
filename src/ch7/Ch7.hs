module Ch7 where

-- ch5 had the TimeMachine type but I don't think we need it

import           Data.List

-- exercise 7.1 :
-- time machine busted: can jump 1 year back, 3 years forward, or 5 years fwd
-- figure out the possible years after three jumps
-- then generalize

brokenThreeJumps :: Int -> [Int]
brokenThreeJumps year = nub $ do
  j1 <- jump year
  j2 <- jump j1
  jump j2
 where
  jumps = [subtract 1, (+ 3), (+ 5)]
  jump y = map ($ y) jumps


-- works but fugly?

brokenJumps :: Int -> Int -> [Int]
brokenJumps year numJumps = nub $ doJumps (jump year) (numJumps - 1)
 where
  jumps = [subtract 1, (+ 3), (+ 5)]
  jump y = map ($ y) jumps
  doJumps years numJumps' = if numJumps' <= 0
    then years
    else do
      y <- years
      doJumps (jump y) (numJumps' - 1)

-- better?

brokenJumpsFold :: Int -> Int -> [Int]
brokenJumpsFold year numJumps = nub
  $ foldl' (\acc _ -> doJumps acc) [year] (replicate numJumps [])
 where
  jumps = [subtract 1, (+ 3), (+ 5)]
  jump y = map ($ y) jumps
  doJumps years = do
    y <- years
    jump y


check :: Int -> Bool
check year = brokenJumps year 3 == brokenThreeJumps year
