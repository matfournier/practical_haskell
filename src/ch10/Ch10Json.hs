{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch10Json where

import           Data.Aeson
import           Data.Text
import           Data.Aeson.Types
import           Control.Applicative
import           GHC.Exts

import           Data.Conduit
import qualified Data.Conduit.Binary           as B
import qualified Data.Conduit.List             as L
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString               as BS
import qualified Data.List                     as DT
import qualified Data.HashMap.Strict           as M

-- Data types

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)
                       -- Eq and Ord will be introduced in Chapter 4

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Eq, Ord)
--- ch 10 here

clientToJSON :: Client Integer -> Value
clientToJSON (GovOrg i n) = object
  [ "type" .= String "govorg"
  , "id" .= Number (fromInteger i)
  , "name" .= String (pack n)
  ]
clientToJSON (Company i n p d) = object
  [ "type" .= String "company"
  , "id" .= Number (fromInteger i)
  , "name" .= String (pack n)
  ]
clientToJSON (Individual i p) = object
  [ "type" .= String "individual"
  , "id" .= Number (fromInteger i)
  , "person" .= personToJSON p
  ]

personToJSON :: Person -> Value
personToJSON (Person f l) =
  object ["first" .= String (pack f), "last" .= String (pack l)]

-- the above sucks but I know we will autoderive it later
-- ditto for the below.  We need to check the HashMap for keys
-- http://hackage.haskell.org/package/aeson-utils-0.3.0.2/docs/Data-Aeson-Utils.html
jsonToPerson :: Value -> Maybe Person
jsonToPerson (Object o) = do
  String f <- M.lookup "first" o
  String l <- M.lookup "last" o
  return $ Person (unpack f) (unpack l)
jsonToPerson _ = Nothing

-- can also use lenses
-- see Chris Penner's Lenses by example for definitive treatment


-- now the above sucks, should use a JSON parser
-- similar to the one from Attoparsec
--  and also uses Applicative

-- note: empty comes from Alternative
jsonToPerson' :: Value -> Parser Person
jsonToPerson' (Object o) = Person <$> o .: "first" <*> o .: "last"
jsonToPerson' _          = Control.Applicative.empty

-- we don't usually define these alone
-- but as a ToJSON and FromJSON typeclass

instance ToJSON Person where
  toJSON = personToJSON

instance FromJSON Person where
  parseJSON = jsonToPerson'


jsonToClient :: FromJSON i => Value -> Parser (Client i)
jsonToClient (Object o) = case M.lookup "type" o of
  Just (String "govorg") -> GovOrg <$> o .: "id" <*> o .: "name"
  Just (String "company") ->
    Company <$> o .: "id" <*> o .: "name" <*> o .: "person" <*> o .: "duty"
  Just (String "individual") -> Individual <$> o .: "id" <*> o .: "person"
  _                          -> Control.Applicative.empty
jsonToClient _ = Control.Applicative.empty

instance ToJSON (Client Integer) where
  toJSON = clientToJSON
instance FromJSON i => FromJSON (Client i) where
  parseJSON = jsonToClient

-- now after converting to/from aeson types
-- need to convert them to actual text
-- ENCODE -> generates text
-- DECODE: options
--   (1) decode takes a ByteString, parses text to produce aeson value
--       converts that value into some other type using FromJSON
--       wraps in a maybe
--   (2) eitherDeCode as above but Left / Right instead
--   (3) perform the two steps separately. First aeson provides json
--       an attoparsec Parser from ByeString to value
--      then you can call fromJSON to gen. the final value

-- note: aeson uses LAZY BYTESTRINGS
-- but conduit and others use STRICT BYTESTRINGS
-- you need to convert if you want to use those


-- Exercise 10-3
-- write ToJSON and FromJSON instances of the Product and Purchase types
-- that were introduced in Exercise 10-1.  Then using aeson and conduit,
-- create an application that reads a list of products [from a file I'm
-- assuming] and shows the average price

data Product  = Product  { id :: Int, name :: String, price :: Double
                         , description :: String }
data Purchase = Purchase { client :: Client Int, products :: [Product] }

clientToJSONi :: Client Int -> Value
clientToJSONi (GovOrg i n) = object
  [ "type" .= String "govorg"
  , "id" .= Number (fromIntegral i)
  , "name" .= String (pack n)
  ]
clientToJSONi (Company i n p d) = object
  [ "type" .= String "company"
  , "id" .= Number (fromIntegral i)
  , "name" .= String (pack n)
  ]
clientToJSONi (Individual i p) = object
  [ "type" .= String "individual"
  , "id" .= Number (fromIntegral i)
  , "person" .= personToJSON p
  ]

-- weird. You don't actually need Purchase in order to do the problem.

jsonToProduct :: Value -> Parser Product
jsonToProduct (Object o) =
  Product <$> o .: "id" <*> o .: "name" <*> o .: "price" <*> o .: "description"

-- that realToFrac as a bitch to find
-- not sure if there is something more useful in here
-- either http://hackage.haskell.org/package/aeson-utils-0.3.0.2/docs/Data-Aeson-Utils.html
productToJson :: Product -> Value
productToJson (Product i n p d) = object
  [ "id" .= Number (fromIntegral i)
  , "name" .= String (pack n)
  , "price" .= Number (realToFrac p)
  , "description" .= String (pack d)
  ]


instance ToJSON Product where
  toJSON = productToJson

instance FromJSON Product where
  parseJSON = jsonToProduct


jsonToPurchase :: Value -> Parser Purchase
jsonToPurchase (Object o) = Purchase <$> o .: "client" <*> o .: "products"

purchaseToJson :: Purchase -> Value
purchaseToJson (Purchase c p) = object
  [ "client" .= clientToJSONi c
  , "products" .= Array (fromList $ fmap productToJson p)
  ]


instance ToJSON Purchase where
  toJSON = purchaseToJson

instance FromJSON Purchase where
  parseJSON = jsonToPurchase

-- winnersFile :: (Monad m, MonadIO m)
--             => ConduitT BS.ByteString BS.ByteString m ()
-- winnersFile = do
--   client <- await
--   case client of
--     Nothing -> return ()
    -- Just c  -> do (w :: Bool) <- liftIO $ randomIO
    --               (y :: Int ) <- liftIO $ randomRIO (0, 3000)
    --               yield $ c <> B

  -- main = runConduit $
  --        B.sourceFile "clients.db" .| B.lines .| winnersFile
  --                                  .| B.sinkFile "clientsWinners.db"

-- saveClients :: FilePath -> [Client Integer] -> IO ()
-- saveClients fPath clients = runConduitRes $
--   yield (toJSON clients) .| L.map (LB.toStrict . encode)
--                          .| B.sinkFile fPath

avgForClient :: Purchase -> Double
avgForClient (Purchase _ products) =
  let n      = fromIntegral $ DT.length products
      totals = sum $ price <$> products
  in  totals / n


-- this works if the json is all on the same line
-- but if the json is broken over several lines this will return
-- null null null null
-- I think the correct thing to use is conduit-attoparsec
-- see this SO here: https://stackoverflow.com/questions/19511678/conduit-with-aeson-attoparsec-how-to-exit-cleanly-without-exception-once-sour


main :: IO ()
main =
  runConduitRes
    $  B.sourceFile "ch10.json"
    .| B.lines
    .| L.map (LB.fromStrict)
    .| L.map (\x -> (fmap avgForClient $ decode x))
    .| L.map (LB.toStrict . encode)
    .| B.sinkFile "ch10.out.json"
