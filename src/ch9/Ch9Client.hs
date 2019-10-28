{-# LANGUAGE OverloadedStrings #-}

import           Network.Socket
import           System.Environment
import           Data.Conduit.Network
import qualified Data.ByteString.Char8         as BS
import qualified Data.Conduit.Binary           as Bool
import           Data.Conduit
import           Control.Monad
import           Control.Monad.Trans


main :: IO ()
main = withSocketsDo $ do
  (name : _) <- getArgs
  runTCPClient (clientSettings 8900 "127.0.0.1") (clientApp name)

clientApp :: String -> AppData -> IO ()
clientApp name d = do
  runConduit $ (yield $ BS.pack name) .| appSink d
  runConduit
    $  appSource d
    .| (do
         Just w <- await
         lift $ BS.putStrLn w
       )
