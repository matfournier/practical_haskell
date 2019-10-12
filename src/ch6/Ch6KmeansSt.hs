{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Ch6KmeansSt where


-- exercise 6-7 K-MEANS USING ST

import           Data.List
import qualified Control.Monad.ST              as ST
import qualified Data.Map                      as M
import           Data.STRef

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid lst =
    let (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) lst
        n      = fromIntegral $ length lst
    in  (u / n, v / n)
-- need to specify how to translate one item in data into it's corresponding vector
-- tyhe type of items to convert and the type of the vectors in
-- which they are translated


class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

data Settings e v = Settings { i :: Int -> [e] -> [v]
                             , k :: Int
                             , threshold :: Double
                             }

data KMeansSt s v = KMeansSt
  { centroids :: STRef s [v]
  , steps :: STRef s Int
  }

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids m = (M.elems . fmap (\x -> (centroid . map toVector) x)) m

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
  in  foldr
        (\p m ->
          let chosenC = minimumBy (compareDistance p) centrs
          in  M.adjust (p :) chosenC m
        )
        initialMap
        points
 where
  compareDistance p x y =
    compare (distance x $ toVector p) (distance y $ toVector p)

kMeans :: (Vector v, Vectorizable e v) => Settings e v -> [e] -> ([v], Int)
kMeans (Settings i k threshold) points = kMeans' (i k points) points threshold

-- todo clean this up w/ a record holding the state type

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> ([v], Int)
kMeans' centroids points threshold = ST.runST $ do
  steps       <- newSTRef 0
  stCentroids <- newSTRef centroids
  kMeans'' stCentroids steps
  v1 <- readSTRef stCentroids
  s1 <- readSTRef steps
  return (v1, s1)
 where
  kMeans'' stC st = do
    prevCentrs <- readSTRef stC
    let assignments = clusterAssignments prevCentrs points
        newCentrs   = newCentroids $ clusterAssignments prevCentrs points
    writeSTRef stC newCentrs
    modifySTRef' st (+ 1)
    let err = sum $ zipWith distance prevCentrs newCentrs
    if err < threshold then return () else kMeans'' stC st
