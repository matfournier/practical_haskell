
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}

module Ch9files where

import           Data.Conduit
import           Data.Conduit.List             as L
import           Control.Monad.State
import qualified Data.ByteString.Char8         as BS
import qualified Data.Conduit.Binary           as B
import           Data.Monoid
import           System.Random


-- sourceFile and sourceHandle generate a stream from a file
-- sinkFile and sinkHandle consume a file
-- those ending in File take care of opening and closing a file
-- those ending in handle expect an already open file and do not close the file

winnersFile :: (Monad m, MonadIO m) => ConduitT BS.ByteString BS.ByteString m ()
winnersFile = do
  client <- await
  case client of
    Nothing -> return ()
    Just c  -> do
      (w :: Bool) <- liftIO randomIO
      (y :: Int ) <- liftIO $ randomRIO (0, 3000)
      yield $ c <> BS.pack (" " ++ show w ++ " " ++ show y) <> BS.pack "\n"
      winnersFile
