{-# LANGUAGE ScopedTypeVariables #-}

module Ch9 where
import           Control.Monad.Loops
import           System.Random
import           Data.Char

data Person = Person { _firstName :: String, _lastName :: String } deriving (Eq, Show)

boop :: IO ()
boop = do
  putStrLn "where do you want to travel?"
  place <- getLine
  let year = (length place) * 10
  putStrLn $ "you should travel to year " ++ show year

boop' :: IO ()
boop' = do
  putStrLn "first name?"
  fName <- getLine
  putStrLn "last name?"
  lName <- getLine
  putChar '>' >> putChar ' '
  print $ Person fName lName

-- repeat 2x upper
upperBoop :: IO ()
upperBoop = do
  upperS <- fmap (map toUpper) getLine
  putStrLn upperS >> putStrLn upperS

actionDo :: IO ()
actionDo = do
  actionName <- getLine
  case lookup actionName listOfActions of
    Just action -> action
    Nothing     -> putStrLn "unknown action"

listOfActions :: [(String, IO ())]
listOfActions =
  [ ( "greet"
    , do
      name <- getLine
      putStrLn $ "Hello" ++ name
    )
  , ( "sum"
    , do
      putStrLn "first number: "
      n1 <- fmap read getLine
      putStrLn "second number: "
      n2 <- fmap read getLine
      putStrLn $ show n1 ++ "+" ++ show n2 ++ "=" ++ show (n1 + n2)
    )
  ]


randomDo = do
  (initial :: Int) <- fmap read getLine
  jumps            <- unfoldrM
    (\_ -> do
      next <- randomRIO (0, 3000)
      if next == initial then return Nothing else return $ Just (next, next)
    )
    initial
  print $ take 10 jumps


-- exercise 9-1