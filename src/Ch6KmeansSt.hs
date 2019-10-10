{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Ch6KmeansSt where


import qualified Control.Monad.ST              as ST
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


kMeans :: (Vector v, Vectorizable e v) => Settings e v -> [e] -> ([v], Int)
kMeans (Settings i k threshold) points = kMeans' (i k points) points threshold

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> ([v], Int)
kMeans' centroids points threshold = _todo
