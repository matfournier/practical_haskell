{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}

module Ch10Atto where

import           Control.Applicative
import           Data.Text
import           Data.Attoparsec.Text



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
