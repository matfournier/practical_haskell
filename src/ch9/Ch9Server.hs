{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans
import qualified Data.ByteString.Char8         as BS
import           Data.Conduit
import           Data.Monoid
import           System.Random
import           Data.Conduit.Network
import           Network.Socket
import           Data.Conduit.Network


isWinner :: ConduitT BS.ByteString BS.ByteString IO ()
isWinner = do
  client <- await
  case client of
    Nothing -> return ()
    Just c  -> do
      lift $ BS.putStrLn c
      (w :: Bool) <- liftIO $ randomIO
      (y :: Int ) <- liftIO $ randomRIO (0, 3000)
      yield $ c <> BS.pack (" " ++ show w ++ " " ++ show y ++ "\n")
      isWinner

-- next step create a conduit which will tie the input flow of the data
-- to the server and output to each client


serverApp :: AppData -> IO ()
serverApp d = runConduit $ appSource d .| isWinner .| appSink d


main :: IO ()
main = withSocketsDo $ runTCPServer (serverSettings 8900 "*") serverApp
