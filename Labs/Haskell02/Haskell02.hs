module Haskell02 where

import Text.Parsec
import Text.Parsec.String

import qualified Data.Map as Map

-- #TODO: put your macid in the following string
macid = "yunc5"

{- Language Specification
 - ------------------------------------------------------------------------------------------------------------
 -     Type Encoding                               |     String Representation
 - ------------------------------------------------------------------------------------------------------------
-      [("ident1",0),("ident2",1),("ident3",2)]    |     { ident1 : 0, ident2 : 1, ident3 : 2 }
 -     []                                          |     { }
 - ------------------------------------------------------------------------------------------------------------
 - Description: the String represented dictionary is composed of identifiers and integers seperated by colons.
 -              Each identifier is composed of alpha-numeric characters (alphabet letters and numbers), and
 -              integers are standard integers (negative and positive). Each entry in the dictionary is
 -              seperated by a comma and all enclosed in curly braces
 -}

{- Task 1:                          (1 Mark)
 -     Complete the following function
 - ------------------------------------------------------------------------------------------------------------
 -     identifier parses a single alpha-numeric identifier (i.e each character must be either a number of
 -     alphabetic letter). It must parse at least a single character or fail
 -     Hint: use the alphaNum parser from Text.Parsec.Char
 - ------------------------------------------------------------------------------------------------------------
 -}
identifier :: Parser String
identifier = do {cs <- many1 alphaNum ; return cs}
{- Task 2:                          (1 Mark)
 -     Complete the following function
 - ------------------------------------------------------------------------------------------------------------
 -     item parses an identifer and an integer seperated by a colon and returns them in a tuple. The colon may
 -     or may not be padded by spaces
 - ------------------------------------------------------------------------------------------------------------
 -}
item :: Parser (String,Integer)
item = do { a <- (try (many1 space)) <|> return "";
            xs <- identifier;
            z <- (try (many1 space)) <|> return "";
            char ':' ;
            y <- (try (many1 space)) <|> return "";
            i <- integer;
            b <- (try (many1 space)) <|> return "";
            return (xs,i) }


{- Task 3:                          (1 Mark)
 -     Complete the following function
 - ------------------------------------------------------------------------------------------------------------
 -     items takes a Parser (item) and parses zero or more occurances of the Parser seperated by commas. The
 -     commas may or may not be padded by spaces
 -     Hint: check out the sepBy function in Text.Parsec
 - ------------------------------------------------------------------------------------------------------------
 -}
items :: Parser (String,Integer) -> Parser [(String,Integer)]
items p = do { char '{';
               xs <- sepBy p (symbol ",");
               char '}';
               return xs }

{- Task 4:                          (1 Mark)
 -     Complete the following function
 - ------------------------------------------------------------------------------------------------------------
 -     intDict parses items contained within curly braces, return a list of tuples.
 -     Hint: modify the parens function to use curly braces, and use items with the item Parser
 - ------------------------------------------------------------------------------------------------------------
 -}
intDict :: Parser [(String,Integer)]
intDict = parens $ items item -- #TODO replace me

{- Task 5:                          (1 Mark)
 -     Complete the following function
 - ------------------------------------------------------------------------------------------------------------
 -     parseDict uses intDict to parse a String (using the parse function from Text.Parsec), and returns either
 -     an error or a Map of Strings and Integers
 -     Hint: pattern match and use Map to convert from a list to a Map
 - ------------------------------------------------------------------------------------------------------------
 -}
parseDict :: String -> Map.Map String Integer
parseDict ss = let p =
                    case parse intDict "" ss of
                        Left err -> error $ "Parse Error: " ++ show err
                        Right se -> se
               in Map.fromList p


{- ------------------------------------------------------------------------------------------------------------
 - Utility Combinators:
 -         Feel free to make use of these as much as you like
 - ------------------------------------------------------------------------------------------------------------
 -}
parens :: Parser a -> Parser a
parens p = do { symbol "(";
                cs <- p;
                symbol ")";
                return cs }

symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits
