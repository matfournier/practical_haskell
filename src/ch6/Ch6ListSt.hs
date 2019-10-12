module Ch6ListSt where

import qualified Control.Monad.ST              as ST
import           Data.STRef


-- cool!

listLength :: [a] -> Integer
listLength list = ST.runST $ do
  l <- newSTRef 0
  traverseList list l
  readSTRef l
 where
  traverseList []       _ = return ()
  traverseList (_ : xs) l = do
    modifySTRef' l (+ 1)
    traverseList xs l
