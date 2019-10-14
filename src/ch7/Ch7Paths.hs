module Ch7Paths where

import           Control.Monad
import           Control.Monad.Logic



-- graph given by list of edges (start, end)
-- assume no self loops (no edges (y,y) with the same year y in both
--   components)
-- list of paths, each represented by a list of years, between the node start
--  and the nod eend
-- thus you need the graph and two nodes and the result will be all the paths
--  between those two nodes

pathsBad :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsBad edges start end = do
  (e_start, e_end) <- edges
  guard $ e_start == start
  subpath <- paths edges e_end end
  return $ start : subpath

-- not quite right: the above doesn't manage the case in which
-- you have reached the target year of your journey


paths :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do
        (e_start, e_end) <- edges
        guard $ e_start == start
        subpath <- paths edges e_end end
        return $ start : subpath
  in  if start == end then return [end] `mplus` e_paths else e_paths


--           2013----1004
--            |      /
--           501    /
--            |    /
--           2558-

graph1 :: [(Int, Int)]
graph1 = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]

-- paths graph1 2013 2558
--  two results: 2013-501-2558
--               2013-1004-2558

graph2 :: [(Int, Int)]
graph2 = [(2013, 501), (501, 2558), (501, 1004), (1004, 501), (2013, 2558)]

-- the list of paths between 2013 and 2558
-- is infinite.
-- you can always loop more between years
-- 501 and 1004

-- but you can get a finite set of those paths

-- take 3 $ paths graph2 2013 2558

-- problem here is that the path [2013, 2558] will never be found
-- because the search strategy in the list monad will find all the subpaths
-- that go through year 501 first.
-- this is because every time the list is taversed it's done in the order
-- that they appear
-- but the subpaths are infinite so we never get to the other one

-- Lgoic monad behaves as the list one sorta


pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths = do
        (e_start, e_end) <- choices edges
        guard $ e_start == start
        subpath <- pathsL edges e_end end
        return $ start : subpath
  in  if start == end then return [end] `mplus` e_paths else e_paths

choices :: [a] -> Logic a
choices = msum . map return

-- the only addition is a call to the newly defined choices function

-- this gives you the same behavior as before
-- observeMany 3 (pathsL graph2 2013 2558)


-- exercise 7-4 desugaring monad notation
-- remember: f >> g = do f
--                       g
-- the second doesn't consume any input from the first


pathsL' :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL' edges start end =
  let e_paths = choices edges >>= \(e_start, e_end) ->
        guard (e_start == start) >> pathsL' edges e_end end >>= \subpath ->
          return $ start : subpath
  in  if start == end then return [end] `mplus` e_paths else e_paths

-- [1,2] `mplus` [3,4]
--         [1,2,3,4]
-- [1,2] `interleave` [3,4]
--         [1,3,2,4]

-- can also write a version where >>= is replaced with >>-
-- which is a fair replacement for >>=


pathsLFair :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsLFair edges start end =
  let e_paths = choices edges >>- \(e_start, e_end) ->
        guard (e_start == start) >> pathsL' edges e_end end >>- \subpath ->
          return $ start : subpath
  in  if start == end then return [end] `interleave` e_paths else e_paths
