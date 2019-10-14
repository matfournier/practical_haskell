module Ch7 where

-- ch5 had the TimeMachine type but I don't think we need it

import           Data.List
import           Control.Monad
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Control.Monad

-- exercise 7.1 :
-- time machine busted: can jump 1 year back, 3 years forward, or 5 years fwd
-- figure out the possible years after three jumps
-- then generalize

brokenThreeJumps :: Int -> [Int]
brokenThreeJumps year = nub $ do
  j1 <- jump year
  j2 <- jump j1
  jump j2
 where
  jumps = [subtract 1, (+ 3), (+ 5)]
  jump y = map ($ y) jumps


-- works but fugly?

brokenJumps :: Int -> Int -> [Int]
brokenJumps year numJumps = nub $ doJumps (jump year) (numJumps - 1)
 where
  jumps = [subtract 1, (+ 3), (+ 5)]
  jump y = map ($ y) jumps
  doJumps years numJumps' = if numJumps' <= 0
    then years
    else do
      y <- years
      doJumps (jump y) (numJumps' - 1)

-- better?

brokenJumpsFold :: Int -> Int -> [Int]
brokenJumpsFold year numJumps = nub
  $ foldl' (\acc _ -> doJumps acc) [year] (replicate numJumps [])
 where
  jumps = [subtract 1, (+ 3), (+ 5)]
  jump y = map ($ y) jumps
  doJumps years = do
    y <- years
    jump y


check :: Int -> Bool
check year = brokenJumps year 3 == brokenThreeJumps year



-- continuing on

broken1 :: Integer -> [Integer]
broken1 n = [n - 1, n + 1]

broken2 :: Integer -> [Integer]
broken2 n = [1024, n + 2]

-- broken1 73 `mplus` broken2 73


-- guard :: MonadPlus m => Bool -> m() --for list, Bool -> [()]
-- guard True = return () --for list [()]
-- guard False = mzero    --for list, []

-- do guard condition
--   return 1

-- guard condition >>= (\_ -> return 1)

-- concat $ map (\_ => [1]) (guard condition)

-- if the guard condiion is satisfied
--    this becomes catcat $ map (\_ -> [1])[()] = concat [[1]] = [1]

-- if the guard condtion is not then the guard condition is equal to the empty list
-- which means mapping will me mapping over the empty list

-- can get fallback behaviors from `mplus`

-- Nothing `mplus` Just 5 will return Just 5
-- Just "first" `mplus` Just "second" will return Just("First")

-- Excercise 7.2
-- write a find_ function with type (a -> bool) -> [a] -> Maybe a that returns the first element
-- in the list that fulfills the given condiiton. Do so using the function msum introduced
-- in this section and the behavior of the MonadPlus instance of Maybe

-- this traverses the whole list though but doesn't evaluate each inner function
-- I think because of laziness?

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ f as =
  let finder f = map (\a -> if f a then Just a else Nothing)
  in  msum $ finder f as

-- find_ (>= 2) [1,2,3,4,5]


--Clients

data Client = GovOrg     { clientName :: String }
            | Company    { clientName :: String
                         , person :: Person
                         , duty :: String }
            | Individual { person :: Person } deriving (Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String, gender:: Gender }
            deriving (Show, Eq, Ord)

data Gender = Male | Female | UnknownGender deriving (Show, Eq, Ord)

-- products

data Product = Product { productId :: Integer, productType :: ProductType  }
             deriving (Show, Eq, Ord)

data ProductType = TimeMachine | TravelGuide | Tool | Trip
                 deriving (Show, Eq, Ord)

data Purchase = Purchase { client :: Client, products :: [Product] } deriving (Show, Eq, Ord)


data PurchaseInfo = InfoClientKind ClientKind
  | InfoClientDuty String
  | InfoClientGender Gender
  | InfoPurchasedProduct Integer
  | InfoPurchasedProductType ProductType
  deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord)

-- note: there is a point-free thing going on here, the [products] is silent
productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo = foldr
  (\(Product i t) pinfos -> S.insert (InfoPurchasedProduct i)
    $ S.insert (InfoPurchasedProductType t) pinfos
  )
  S.empty

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c p) =
  Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo p


-- Excercise 7-3 Clients into Items
-- Write the missing clientToPurchaseInfo function. It should have the type
--  Client -> Set PurchaseInfo
-- and should satisfy the requirements for obtaining items from clients that
-- I explained while defining the PurchaseInfo dataType.  Here's an example
-- showing the return value for a test

-- > clientToPurchaseInfo (Company "1984 Inc." (Person "George" "Orwell" Male) "Director")
-- fromList [InfoClientKind kindCompany, InfoClientDuty "Director", InfoClientGender Male]

clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo c = S.fromList $ info c
 where
  info (GovOrg _) = [InfoClientKind KindGovOrg]
  info (Company _ (Person _ _ gender) duty) =
    [InfoClientKind KindCompany, InfoClientDuty duty, InfoClientGender gender]
  info (Individual (Person _ _ gender)) =
    [InfoClientKind KindIndividual, InfoClientGender gender]




-- I got confused, so went reading:

-- Let I = {i1, i2, i3, ..., in} be a set of attributes called items
-- let D = {t1, t2, t3, ...., tn} be a se of transactions
--       = a database, every tn has a unique id and contains
--         a subset of I

-- a rule X -> Y when X and Y are subets of I and they contain no element
--   in common

-- Supp (x) = the proportion of transactions in the databsae in which item X appears
-- confidence = Conf (X -> Y) = Supp (X union Y) / Supp (X)
--   this is P (Y | X) - liklihood of item Y being purchased when Item X is
--                       is purchased


newtype FrequentSet = FrequentSet (Set PurchaseInfo) deriving (Eq, Ord)

data AssocRule = AssocRule (Set PurchaseInfo) (Set PurchaseInfo) deriving (Eq, Ord)

instance Show AssocRule where
  show (AssocRule a b) = show a ++ " => " ++ show b


-- we need helper fns for computing confidence and support measures
-- for frequent sets and association rules
-- dubious direction: thinking about how to increase the speed of the following
--   code is a good exercise

setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) =
  let total = length trans
      f (Transaction tElts) = sElts `S.isSubsetOf` tElts
      supp = length (filter f trans)
  in  fromIntegral supp / fromIntegral total

ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence trans (AssocRule a b) =
  setSupport trans (FrequentSet $ a `S.union` b)
    / setSupport trans (FrequentSet a)


-- gen initial set of one element that are frequent

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions = noDups $ do
  Transaction t <- transactions
  e             <- S.toList t
  let fs = FrequentSet $ S.singleton e
  guard $ setSupport transactions fs > minSupport
  return fs

-- noDups removes duplicates in a list
-- this is faster than using nubs from prelude
noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateNextLk
  :: Double
  -> [Transaction]
  -> (Int, [FrequentSet])
  -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk minSupport transactions (k, lk) =
  let lk1 = noDups $ do
        -- if lk = [1,2,3]
        -- this would make [1, 1], [1, 2], [1,3], [2, 1], [2, 2], [2, 3].. etc
        FrequentSet a <- lk
        FrequentSet b <- lk
        guard $ S.size (a `S.intersection` b) == k - 1
        let fs = FrequentSet $ a `S.union` b
        guard $ setSupport transactions fs > minSupport
        return fs
  in  Just (lk1, (k + 1, lk1))

-- we run this like so concat $ unfoldr (generateNextLk minSupport transactions) (1, l1)

-- final step is to generate association rules w/ a min. confidence
--   take a frequent Set I from the prev step
--   then partition the set into two disjoint sets A and B such that A is nonempty
--   Each partition will give rise to a rule A => B which should be checked for
--     the min. confidence.
--   this should be done for each possible partition of each frequent set
--     we do this via the powerset function

generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets = do
  FrequentSet fs <- sets
  subset@(_ : _) <- powerset $ S.toList fs
  let ssubset = S.fromList subset
      rule    = AssocRule ssubset (fs `S.difference` ssubset)
  guard $ ruleConfidence transactions rule > minConfidence
  return rule

powerset :: [a] -> [[a]]
powerset []       = [[]]
powerset (x : xs) = powerset xs ++ map (x :) (powerset xs)

apriori :: Double -> Double -> [Transaction] -> [AssocRule]
apriori minSupport minConfidence transactions =
  generateAssocRules minConfidence transactions $ concat $ unfoldr
    (generateNextLk minSupport transactions)
    (1, generateL1 minSupport transactions)


-- I don't understand what the point of the apriori example is
-- there doesn't appear to be exercises to do?
-- although it's an interesting use of do notation and guards I guess?
