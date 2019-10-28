{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}

module Ch9Conduit where

import           Data.Conduit
import           Data.Conduit.List             as L
import           Control.Monad.State
-- three kinds of actors
--  1) sources
--     -------------
--     provide streams of values to be consumed.  E.g. text from a data file
--     reading from a network connection, obtain elem of a list
--
--  2) sinks
       ---------------
--     consuming sources and not producing any further stream elements
--
--  3) transformers
--     -------------
--     consume input stream and produce an output stream

-- each actor can take care of acquiring and releasing resources in a safe
-- and predictable way
-- o
-- e.g. write to fil sink may open a handle when the stream starts and close
-- the handle when the input stream finishes

-- Conduit only exposes a single type ConduitT i o m r
--  i - type of values of the stream
--  o - the type of the output stream
--  m - which side effects may occur
--  r - type of the final result
--  the trick to only need one type is to set an arguement to Void when no output
--  is generated, or to () if no input is required or no interesting result is
--  produced.

-- a complete flow of data is est. by (.|) which means `connect` or `fuse`
-- you can only combine two ConduitT's if the output type of one matches the input
-- type of the other.
-- to run all the operations in a stream you call runConduit or runConduitPure if no side
--   effects are involved.

-- let c = L.sourceList [1..4] .| L.fold (+) 0
-- runConduitPure c

-- you can also generate streams using `unfold`

-- runConduitPure $ L.unfold (\x -> Just (x, x+1)) 1 .| L.isolate 10 .| L.consume
-- isolate is filter
-- consume turns back into a list

data Person = Person { _firstName :: String, _lastName :: String } deriving (Eq, Show, Ord)

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)

-- await tries to take next element in the input stream. If it's successful it's returned
-- wrapped in Just if it's Nothing the stream ends

-- yield is the fn used to send values to the output stream


people :: Monad m => ConduitT (Client i) Person m ()
people = do
  client <- await
  case client of
    Nothing -> return ()
    Just c  -> do
      case c of
        Company { person = p }    -> yield p
        Individual { person = p } -> yield p
        _                         -> return ()
      people

-- important to notiec if you wrap some monad m in a ConduitT the result of the
-- streaming data will live inside such monad m

countGovOrgs :: MonadState Int m => ConduitT (Client i) Void m Int
countGovOrgs = do
  client <- await
  case client of
    Nothing -> do
      n <- lift get
      return n
    Just c -> do
      case c of
        GovOrg{} -> lift $ modify (+ 1)
        _        -> return ()
      countGovOrgs



-- Exercise 9.4 Implement unfold
-- this took me forever to figure out for some reason.
-- the `yield a >>= gen newState` was very unintuitive
-- since all the examples are more like sinks and this is the first source example

myUnfold :: Monad m => (b -> Maybe (a, b)) -> b -> ConduitT i a m ()
myUnfold f = gen
 where
  gen state = case f state of
    Just (a, newState) -> yield a >>= (\_ -> gen newState)
    Nothing            -> return ()

--runConduitPure $ myUnfold (\x -> Just(x, x+1)) 1 .| L.isolate 10 .| L.consume

-- Exercise 9.4 implement filter

myFilter :: Monad m => (a -> Bool) -> ConduitT a a m ()
myFilter f = do
  maybeV <- await
  case maybeV of
    Nothing -> return ()
    Just v  -> do
      if f v then yield v else return ()
      myFilter f

-- Exercise 9.4 implement fold
-- took me awhile, in the last line I had `f acc v in yield updatedAcc` which would
-- have been scan, not fold.  Once I clued in with the compiler error I got it

myFold :: Monad m => (b -> a -> b) -> b -> ConduitT a o m b
myFold f = go
 where
  go acc = await >>= \maybeV -> case maybeV of
    Nothing -> return acc
    Just v  -> let updatedAcc = f acc v in return () >>= (\_ -> go updatedAcc)
