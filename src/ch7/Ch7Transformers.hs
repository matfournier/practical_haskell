module Ch7Transformers where

import           Control.Monad
import           Control.Monad.Writer
import           Control.Monad.Reader
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
