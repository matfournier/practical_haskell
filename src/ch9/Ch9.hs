{-# LANGUAGE ScopedTypeVariables #-}

module Ch9 where
import           Control.Monad.Loops
import           Control.Monad.Reader
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


data GameConfig = GameConfig { _min :: Int
                             , _max :: Int
                             , _numTry :: Int
                             } deriving (Eq, Ord, Show)


-- this sucks
gameLoop :: GameConfig -> Int -> Int -> IO [Char]
gameLoop cfg@(GameConfig min max stop) target tries = do
  if tries >= stop then doStop else doContinue cfg
 where
  doStop = do
    return "Number of tries exceeded"
  doContinue cfg = do
    putStrLn "guess:"
    guess <- fmap read getLine
    if guess == target
      then return "You win"
      else gameLoop cfg target (tries + 1)


-- better
readerGameLoop :: Int -> Int -> ReaderT GameConfig IO ()
readerGameLoop target n = do
  (GameConfig min max stop) <- ask
  if n >= stop
    then lift
      $ putStrLn ("too many guesses. The right answer was " ++ show target)
    else do
      lift $ putStrLn "guess again"
      (guess :: Int) <- lift $ fmap read getLine
      if guess == target
        then lift $ putStrLn "you win"
        else readerGameLoop target (n + 1)

game :: IO ()
game = do
  let cfg = GameConfig { _min = 3, _max = 17, _numTry = 5 }
  putStrLn
    (  "Guessing game! guess between "
    ++ (show $ _min cfg)
    ++ " and "
    ++ (show $ _max cfg)
    )
  target <- randomRIO (_min cfg, _max cfg)
  runReaderT (readerGameLoop target 0) cfg
