{-# LANGUAGE ScopedTypeVariables #-}
module Ch6writer where

newtype MyWriter m a = MyWriter (a, m) deriving (Show)

instance Functor (MyWriter m) where
  fmap f (MyWriter (a, m)) = MyWriter (f a, m)

instance Monoid m => Applicative (MyWriter m) where
  pure a = MyWriter (a, mempty)
  (<*>) (MyWriter (f, w')) (MyWriter (a, m)) = MyWriter (f a, w' `mappend` m)

instance Monoid m => Monad (MyWriter m) where
  return a = MyWriter (a, mempty)
  MyWriter (a, m) >>= f =
    let (MyWriter (b, m')) = f a in MyWriter (b, m `mappend` m')

tell :: (Monoid m) => m -> MyWriter m ()
tell m = MyWriter ((), m)


logNum :: Int -> MyWriter [String] Int
logNum x = MyWriter (x, ["Got number: " ++ show x])

multWithLog :: MyWriter [String] Int
multWithLog = do
  a <- logNum 3
  b <- logNum 5
  _ <- tell ["yep"]
  return (a * b)

-- applciative
-- pure (+) <*> logNum 3 <*> logNum4
