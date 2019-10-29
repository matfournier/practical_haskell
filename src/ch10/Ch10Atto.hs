{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}

module Ch10Atto where

import           Control.Applicative
import           Data.Text
import           Data.Attoparsec.Text
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy.Builder.Int    as B
import qualified Data.Text.Lazy                as LT


data GreetingYear = GreetingYear Text Int

-- parsers hello2016
greetingYearParser :: Parser GreetingYear
greetingYearParser =
  GreetingYear <$> (string "hello" <|> string "bye") <*> decimal

-- <$> :: (a -> b )       -> Parser a -> Parser b
-- <*> :: Parser (a -> b) -> Parser a -> Parser b

-- what if you want the same pattern as before but with a space between the
-- greeting and the number

greetingYearParserS :: Parser GreetingYear
greetingYearParserS =
  (\g _ y -> GreetingYear g y)
    <$> (string "hello" <|> string "bye")
    <*> char ' '
    <*> decimal

-- but this sucks. We have to put the `_` to stand in for the empty character
-- but we can use `<*` to describe the case where some input should be parsed
-- but not used to build any larger structure

greetingYearParserS' :: Parser GreetingYear
greetingYearParserS' =
  GreetingYear <$> (string "hello" <|> string "bye") <* char ' ' <*> decimal


-- time to build the parser for the Client output generated earlier

-- need to parse names, taking into account rules of escaping
-- \n () replaced by a backslash and the character itself...

-- how these work since it's confusing `\\` is escaping a slash, so it's really \
--  the first let p = fmap (const ',') (string "\\,")
-- if you just had parseOnly (string "\\,") "\\,"
--   you get back "\\," but that's not what you want
--   so you map const `,` over it so that any value you pass to const
--    just gives you `,` e.g. (const ',') "sdhfs" returns ','
aChar :: Parser Char
aChar =
  (const ',')
    <$> (string "\\,")
    <|> (const '\n')
    <$> (string "\\n")
    <|> (const '(')
    <$> (string "\\(")
    <|> (const ')')
    <$> (string "\\)")
    <|> satisfy (notInClass ",\n()")


-- call this until some nonmatching input is found.
-- we could create a new Parser that calls itself recursively
-- in each step it prepends a character to the string val to be returned later
-- but also need a base case which will be applied when a nonmatching character is found
-- the way to create a parser that returns some value, w/o consuming any input is via pure :: a -> Parer a

aString :: Parser String
aString = ((:) <$> aChar <*> aString) <|> (pure "")


-- parseOnly aString "hello\\, my name is mat \\n"

-- Alternative f => f a -> f [a]
aString' = many aChar


data Person = Person { firstName :: String, lastName :: String }
            deriving (Show, Eq, Ord, Read)
data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)

escapeString :: String -> Text
escapeString =
  replace "\n" "\\n"
    . replace "," "\\,"
    . replace "(" "\\("
    . replace ")" "\\("
    . pack

personToText :: Person -> Text
personToText (Person f l) =
  "person(" <> escapeString f <> "," <> escapeString l <> ")"

clientToText :: Client Int -> Text
clientToText (GovOrg i n) =
  "client(gov," <> escapeString (show i) <> "," <> escapeString n <> ")"
clientToText (Company i n p d) =
  "client(com,"
    <> escapeString (show i)
    <> ","
    <> escapeString n
    <> ","
    <> personToText p
    <> ","
    <> escapeString d
    <> ")"
clientToText (Individual i p) =
  "client(ind," <> escapeString (show i) <> "," <> personToText p <> ")"

-- (<$), function application that drops it's first arguement
-- its the same as `fmap  . const`
-- e.g. 2 <$ [1,2,3] == [2,2,2]

aPerson :: Parser Person
aPerson =
  Person <$ string "person(" <*> aString <* char ',' <*> aString <* char ')'

aClient :: Parser (Client Int)
aClient =
  GovOrg
    <$  string "client(gov,"
    <*> decimal
    <*  char ','
    <*> aString
    <*  char ')'
    <|> Company
    <$  string "client(com,"
    <*> decimal
    <*  char ','
    <*> aString
    <*  char ','
    <*> aPerson
    <*  char ','
    <*> aString
    <*  char ')'
    <|> Individual
    <$  string "client(ind,"
    <*> decimal
    <*  char ','
    <*> aPerson
    <*  char ')'

-- let co = Company 1 "Black Hole Inc." (Person "John" "Smith") "Traveller"
-- let b = clientToText co
--   results in
--     "client(com,1,Black Hole Inc.,person(John,Smith),Traveller)"
-- parseOnly aClient b
