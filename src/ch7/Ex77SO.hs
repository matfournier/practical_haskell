{-# LANGUAGE FlexibleContexts #-}

module Ex77SO where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.RWS

graph1 :: [(Int, Int)]
graph1 = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]


-- similar to my solution but it looks like to get it to actually
-- run without the user error mzero exception they have to
-- explicitly put the type. No fucking clue.

pathImplicitRW :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathImplicitRW edges start end = execWriterT rdr
 where
  rdr = runReaderT (pathImplicitStack' start end) edges :: WriterT [Int] [] ()

pathImplicitRWS :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathImplicitRWS edges start end = map snd exec
  where exec = execRWST (pathImplicitStack' start end) edges ()


-- if you run this as runWriterT ((runReaderT 2013 2558) graph1)
-- you will get that user error mzero problem

pathImplicitStack'
  :: (MonadReader [(Int, Int)] m, MonadWriter [Int] m, MonadPlus m)
  => Int
  -> Int
  -> m ()
pathImplicitStack' start end | start == end = tell [end]
pathImplicitStack' start end                = do
  (s0, e0) <- ask >>= msum . map return
  guard $ s0 == start
  tell [start]
  pathImplicitStack' e0 end
