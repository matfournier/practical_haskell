{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ch6 where

import           Data.List
import qualified Data.Map                      as M

--
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

clusterAssignmentPhase
  :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
  in  foldr
        (\p m ->
          let chosenC = minimumBy (\x y -> compareDistance p x y) centroids
          in  M.adjust (p :) chosenC m
        )
        initialMap
        points
 where
  compareDistance p x y =
    compare (distance x $ toVector p) (distance y $ toVector p)

-- for each cluster, convert the list of associated elements to vectors and then get the centroi of this set
-- then convert the map into a list of (old, new) elements
newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase m = op m
 where
  op theMap = (M.toList . fmap (\xs -> (centroid . map toVector) xs)) theMap

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  (foldr (\(x, y) s -> s + distance x y) 0.0 centroids) < threshold

kMeans
  :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) -- initialization function
  -> Int -- # of centroids
  -> [e] -- the information
  -> Double --threshold
  -> ([v], Int)  -- final centroids + num of iterations
kMeans i k points threshold = kMeans' (i k points) points threshold 1

kMeans'
  :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> Int -> ([v], Int)
kMeans' centroids points threshold n =
  let assignments     = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids    = map snd oldNewCentroids
  in  if shouldStop oldNewCentroids threshold
        then (newCentroids, n)
        else kMeans' newCentroids points threshold (n + 1)

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v


-- not sure if the lens section is worth doing TBH
-- perhaps skip?

-- SKIPPED

-- STATE STATE STATE

type State s a = s -> (a, s)


-- fmap :: (a -> b) -> (State s a) -> (State s b)
-- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
newtype State' s a = State' (s -> (a, s))
instance Functor (State' s) where
  fmap f (State' g) = State' $ \s0 -> let (a, s1) = g s0 in (f a, s1)
-- g is a function s -> (a, s)


thenDo :: State s a -> (a -> State s b) -> State s b
thenDo f g s = let (resultOfF, stateAfterF) = f s in g resultOfF stateAfterF

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

kMeans'' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans'' points =
  (\s -> (centroids s, s))
    `thenDo` (\prevCentrs ->
               (\s -> (clusterAssignments prevCentrs points, s))
                 `thenDo` (\assignments ->
                            (\s -> (newCentroids assignments, s))
                              `thenDo` (\newCentrs ->
                                         (\s ->
                                             ((), s { centroids = newCentrs })
                                           )
                                           `thenDo` (\_ ->
                                                      (\s ->
                                                          ( ()
                                                          , s
                                                            { steps = steps s + 1
                                                            }
                                                          )
                                                        )
                                                        `thenDo` (\_ ->
                                                                   (\s ->
                                                                       ( threshold
                                                                         s
                                                                       , s
                                                                       )
                                                                     )
                                                                     `thenDo` (\t ->
                                                                                (\s ->
                                                                                    ( sum
                                                                                      $ zipWith
                                                                                          distance
                                                                                          prevCentrs
                                                                                          newCentrs
                                                                                    , s
                                                                                    )
                                                                                  )
                                                                                  `thenDo` (\err ->
                                                                                             if err
                                                                                                < t
                                                                                             then
                                                                                               (\s ->
                                                                                                 ( newCentrs
                                                                                                 , s
                                                                                                 )
                                                                                               )
                                                                                             else
                                                                                               (kMeans''
                                                                                                 points
                                                                                               )
                                                                                           )
                                                                              )
                                                                 )
                                                    )
                                       )
                          )
             )
