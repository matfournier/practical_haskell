module Ch7Paths where

import           Control.Monad
import           Control.Monad.Logic
import           Control.Monad.Reader
import           Control.Monad.Writer


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

-- Combining values under a monad

-- scala: traverse
-- mapM:  Monad m => (a -> m b) -> [a] -> m [b]

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

-- say we have some simple fn that prepends on string to another
-- but instead of using an extra param, you want to use some global context
-- modeled with the reader Monad

-- if you now have a list of strings you want to prefix with that first string
-- your intuition may be to use map. but..

-- :t map addPrefix
-- map addPrefix :: [String] -> [Reader String String]
-- we get a bunch of readers which is not what we want -- each needs a context

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM addPrefix

-- this makes much more sense.

-- in some cases you don't care about ht eresults of the computation
--   just that effectit has on the things in a list
--   e.g. logging a bunch of data
--   you don't care about the return
-- this is a job for mapM_


logInformation :: [String] -> Writer String ()
logInformation = mapM_ (\s -> tell (s ++ "\n"))

-- runwWriter $ logInformation ["one", "two"]
-- ((), "one\n\two\n")

-- forM and forM_ are the same just arguements reversed
-- so list comes first
-- kinda looks like a foreach


-- exercise 7.5
-- try to write the definition of sequence
-- do it using do notation and pattern matching
-- like with any other list function you should consider the cases of the empty list
-- with some head and tail: remember that x <- "v" extracts the value wrapped in a monad from
-- v : m a into a binding of x :: a
-- for the mapM function, the hints is to write it as a composition of two other functions
-- the ones you should use have already been showcased in the example of prefixing a list
-- of strings with a common prefix from a shared context



-- I have no idea what the hint is. This made sense?

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ []       = return []
mapM' f (x : xs) = do
  fx   <- f x
  rest <- mapM' f xs
  return $ fx : rest

addPrefixL' :: [String] -> Reader String [String]
addPrefixL' = mapM' addPrefix


-- just use mapM' with identity?
sequence' :: Monad m => [m a] -> m [a]
sequence' ma = mapM' id ma

-- as per the instructions
sequence'' :: Monad m => [m a] -> m [a]
sequence'' []       = return []
sequence'' (x : xs) = do
  xUnwrapped <- x
  rest       <- sequence'' xs
  return $ xUnwrapped : rest

-- the behavior of sequence/mapM is mostly determined by
-- the monad

-- there are also folds that work under  amonad e..g foldM
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

-- foldM always preforms a left fold.

-- a ver. of factorial that takes care of the # of folding steps needed for evaluation

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM (\f x -> tell (Sum 1) >> return (f * x)) 1 [1 .. n]

-- filterM
-- filters some value based on some monadic predicate

powerset :: [a] -> [[a]]
powerset == filterM (\_ -> [False, True])

-- monad comprehensions: skipped
