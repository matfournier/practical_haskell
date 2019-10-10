{-# LANGUAGE InstanceSigs #-}

module Ch4 where


import           Data.Tree
import           Data.Graph
import qualified Data.Map                      as M


-- preoder traversal: before subtrees visited

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) =
  let subtreesTraversed = concatMap (preOrder f) subtrees
  in  f v : subtreesTraversed

pictureTree :: Tree Int
pictureTree = Node 1 [Node 2 [Node 3 [], Node 4 [], Node 5 [], Node 6 []]]


timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
  [ ("wood"    , "wood"    , ["walls"])
  , ("plastic" , "plastic" , ["walls", "wheels"])
  , ("aluminum", "aluminum", ["wheels", "door"])
  , ("walls"   , "walls"   , ["done"])
  , ("wheels"  , "wheels"  , ["done"])
  , ("door"    , "door"    , ["done"])
  , ("done"    , "done"    , [])
  ]

-- pg 144
data TravelGuide = TravelGuide { title :: String
                               , authors :: [String]
                               , price :: Double }
                 deriving (Show, Eq, Ord)

data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1 | Leaf1 deriving Show


-- need to find something in there

treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 t (Node1 v l r) = case compare t v of
  EQ -> Just v
  LT -> treeFind1 t l
  GT -> treeFind1 t r
treeFind1 _ Leaf1 = Nothing

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 t n@(Node1 v l r) = case compare t v of
  EQ -> n
  LT -> Node1 v (treeInsert1 t l) r
  GT -> Node1 v l (treeInsert1 t r)
treeInsert1 t Leaf1 = Node1 t Leaf1 Leaf1

-- step 2
-- polymorphic Trees

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a) | Leaf2 deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
  EQ -> Just v
  LT -> treeFind2 t l
  GT -> treeFind2 t r
treeFind2 _ Leaf2 = Nothing

treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) = case compare t v of
  EQ -> n
  LT -> Node2 v (treeInsert2 t l) r
  GT -> Node2 v l (treeInsert2 t r)
treeInsert2 t Leaf2 = Node2 t Leaf2 Leaf2


-- you can't do this because you can't put a typeclass constraint in fmap
-- instance Functor BinaryTree2 where
--   fmap f (Node2 v l r) = treeInsert2 (f v) Leaf2
--   fmap _ Leaf2         = Leaf2

-- also try to implement concatenation of binary trees by repeatly inserting all the elements in one of
-- of the binary trees.

instance Foldable BinaryTree2 where
  foldr f z Leaf2         = z
  foldr f z (Node2 v l r) = f v (foldr f (foldr f z l) r)



treeConcat2 :: Ord a => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
treeConcat2 t1 (Node2 v l r) = treeConcat2 mergedWithLeft r
  where mergedWithLeft = treeConcat2 (treeInsert2 v t1) l
treeConcat2 t1 Leaf2 = t1

-- ord for travelGuide compares title, list of authors, and price
-- but we want to order them by price!

newtype TGByPrice = TGByPrice TravelGuide deriving Eq

instance Ord TGByPrice where
  (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

-- Binary trees w/ monoidal cache
-- still finding the samllest price takes some time
-- include a cache in every node which stores the price of the smallest element of the tree

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c) | Leaf3 deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r) = case compare v v2 of
  EQ -> Node3 v2 c2 l r
  LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
  GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r) = case compare v v2 of
  EQ -> Node3 v2 c2 l r
  LT ->
    let newLeft  = treeInsert4 v c l
        newCache = c2 <> cached newLeft <> cached r
    in  Node3 v2 newCache newLeft r
  GT ->
    let newRight = treeInsert4 v c r
        newCache = c2 <> cached l <> cached newRight
    in  Node3 v2 newCache l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3


cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty
