{-# LANGUAGE FlexibleContexts #-}

module Ch7Transformers where

import           Control.Monad
import           Control.Monad.Writer
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.RWS
import           Data.Foldable
import           Data.List

-- rolling own monad combination
-- a fn that will get all possible paths between two points in a graph
--   but will manage the paths as the output of a Writer monad

pathsWriter :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = map execWriter (pathsWriter' edges start end)

pathsWriter' :: [(Int, Int)] -> Int -> Int -> [Writer [Int] ()]
pathsWriter' edges start end =
  let e_paths = do
        (e_start, e_end) <- edges
        guard $ e_start == start
        subpath <- pathsWriter' edges e_end end
        return $ do
          tell [start]
          subpath
  in  if start == end then tell [start] : e_paths else e_paths

-- book says it uses the Monoid instance of lists and uses it
--  in the Writer.  Not sure what this means as I can't see
--  any use of Monoid
-- oh I see. It's the `tell [start]` part
-- second: the pathsWriter needs to map execWriter every element
--  returned

-- # monad transformer -- combine the effects of several monads into
--   a new one


-- needto lift the lower layer up.  This is provided by the only
-- function of the MonadTrans type class
-- which the class of all monad transformers named lift

-- class MonadTrans t where
--  lift :: Mond m => m a -> t m a

-- if we want to translate the prev. code into using monad transformers
-- you must include a lift in the access to the edges arguement in order
-- to use the do notation

-- e.g. lift edges :: WriterT [Int] [] ()


-- This WORKS and needs to be re-written in exercise 7-7 using
-- MonadReader and MonadWriter

pathsWriterT' :: [(Int, Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
  let e_paths = do
        (e_start, e_end) <- lift edges
        guard $ e_start == start
        tell [start]
        pathsWriterT' edges e_end end
  in  if start == end then tell [start] `mplus` e_paths else e_paths

pathsWriterT :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end)

-- the computational effects depend on the other transformations

-- StateT s [] can rerepsent those nondeterministic computations where
--             each of the paths has a different result and a different
--             internal state
-- ListT (State s) represents those computations where several results
--             can be returned but the state is shared among all the
--             branches


readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do
  x <- ask
  lift . tell $ show x
  return $ x + 1

-- efects are stacked in reverse order:
--   the outer call corresponds to runWriter
--   the inner call is for runReaderT (which peels one layer)

-- runWriter (runReaderT readerWriterExample 3)
-- (4, "3")

-- Exercise 7.6 two states at a time


-- write a fn that compues the factorial of a number
-- instead of the usual implementation, use one based on two states:

-- one for keeping decreasing counter
-- another one for keeping the factorial

-- implement the state using StateT Integer (State Integer) monad
-- thus you must use lift to access one of the internal states
-- the final code to run the monad computation should be
-- similar to execState (execStateT factorial x ) 1

factorialState' :: StateT Int (State Int) ()
factorialState' = do
  n <- get
  lift $ modify (* n)
  modify (subtract 1)
  unless (n == 1) factorialState'

factorialState :: Int -> Int
factorialState n = execState (execStateT factorialState' n) 1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

check :: Int -> Bool
check n = factorial n == factorialState n



readerWriterExampleMTL :: (MonadReader Int m, MonadWriter String m) => m Int
readerWriterExampleMTL = do
  x <- ask
  tell $ show x
  return $ x + 1

-- Exercise 7.7 - SUPER CONFUSED
-- exact problem statement
-- write a new version of pathsWriter that holds the graph in a read-only context.
-- this means you need to use the functionality from both MonadReader (for handling
-- the graph) and MonadWriter (for handling the paths) wrapping the base list monad.
-- to check that the function is general, use two different monads to provide the
-- requested functionality: ReaderT r (writerT w []) a and RWST r w s a.

-- after hours and hours I have no fucking clue
-- using guard it complains about no instance of MonadPlus for identity (there isn't one!)
-- or running into the problem of trying to arbitrarily lift the list monad into the stack
-- doesn't seem to be working.
-- someone on SO had this problem back in 2014:
-- https://stackoverflow.com/questions/24195617/use-list-monad-inside-monad-transformer-type-classes
-- but the solution throws an error ("Exception: user error (mzero)")
-- no clue why this explodes with an exception
pathsWriterMTL'
  :: (MonadReader [(Int, Int)] m, MonadWriter [Int] m, MonadPlus m)
  => Int
  -> Int
  -> m ()
pathsWriterMTL' start end =
  let e_paths = do
        (e_start, e_end) <- ask >>= msum . map return
        guard $ e_start == start
        tell [start]
        pathsWriterMTL' e_end end
  in  if start == end then tell [start] else e_paths

pathsWriterMTL start end graph =
  runWriterT (runReaderT (pathsWriterMTL' start end) graph)


-- trying a version just using stacked monad transformers which works
-- even though the version above throws the exception
-- runWriterT ( runReaderT (paths' 2013 2558) graph1 )
-- but the whole `ask >>= msum . map return` is magic
-- this entire question sucks.

paths' :: Int -> Int -> ReaderT [(Int, Int)] (WriterT [Int] []) ()
paths' start end =
  let e_paths = do
        (e_start, e_end) <- ask >>= msum . map return
        guard $ e_start == start
        tell [start]
        paths' e_end end
  in  if start == end then tell [start] else e_paths


graph1 :: [(Int, Int)]
graph1 = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]

someFunct :: ReaderT [(Int, Int)] [] Int
someFunct = do
  (s0, e0) <- ask >>= msum . map return
  return s0
