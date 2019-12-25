module Ch10App where

-- class functor f => Applicative f where
--   pure :: a -> f a
-- (<*>)  :: f (a -> b) -> f a -> f b

-- <*> extract some fn inside o a context of type f
--   and apply one arguement to it

-- Just (+) <*> Just 2 <*> Just 3
-- (pure (+) <*> pure 2 <*> pure 3) :: Maybe Int

plusThree :: Int -> Int -> Int -> Int
plusThree x y z = x + y + z

-- plusThree <$> Just 1 <*> Just 2 <*> Just 3
-- 6
-- pure plusThree `ap` Just 1 `ap` Just 2 `ap` Just 3
-- 6

-- monad implications:
-- using monads as an arguement to a function can change
-- the fn to be executed in the next step
-- whereas in applicative, arguements to a fn may affect
-- the current step to be executed but not the remaining one s

-- to make it clear: consider the case where you want to use the
-- the following fn that depending on whether the first parameter
-- is 1 or not, would result in adding one or doubling one number

-- \x -> if x == 1 then Just (+1) else Just(*2)

-- if you apply one parameter using <*> you get the following:
--  (\x -> if x == 1 then pure (+1) else pure (*2)) <$> Just 1
--    :: (Num a) => Maybe (Maybe (a -> a))
-- applicative is no help!  you need join from monad to do this.

-- MonadPlus
-- both monad and Alternative
-- has a <|> method: f a -> f a -> f a
-- has a empty : f a method
--   failure via empty
--   choice via <|> , much like mempty and mplus

-- Nothing <|> Just 2 <|> Just 3
--  Just 2

-- Traversable

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = sequenceA . fmap f
--   sequenceA :: Applicative f => f (f a) -> f (t a)
--   sequenceA = traverse id
--   mapM :: Monad m => (a -> m b) -> f a -> m (t b)
--   mapM f = unwrapMonad . traverse (WrapMond . f)
--   sequence :: Monad m => t (m a) -> m (t a)
--   sequence = mapM id
