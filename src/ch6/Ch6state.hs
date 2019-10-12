{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Ch6state where

-- gets example on pg 215 working since it's not in the repo

import           Data.List
import qualified Data.Map                      as M
import qualified Control.Monad.State           as S

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

data KMeansState v = KMeansState
  { centroids :: [v]
  , threshold :: Double
  , steps :: Int
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

kMeans' :: (Vector v, Vectorizable e v) => [e] -> S.State (KMeansState v) [v]
kMeans' points = do
  prevCentrs <- S.gets centroids
  let assignments = clusterAssignments prevCentrs points
  let newCentrs   = newCentroids assignments
  S.modify (\s -> s { centroids = newCentrs })
  S.modify (\s -> s { steps = steps s + 1 })
  t <- S.gets threshold
  let err = sum $ zipWith distance prevCentrs newCentrs
  if err < t then return newCentrs else kMeans' points

kMeans
  :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v])
  -> Int
  -> [e]
  -> Double
  -> [v]
kMeans i n pts t = S.evalState (kMeans' pts) (initializeState i n pts t)


initializeState
  :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v])
  -> Int
  -> [e]
  -> Double
  -> KMeansState v
initializeState i n pts t = KMeansState (i n pts) t 0

info :: [(Double, Double)]
info = [(1, 1), (1, 2), (4, 4), (4, 5)]

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v
