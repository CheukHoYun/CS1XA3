{-|
Module : ExprParser
Description : The parser module used to accept user input strings and turn them into operatable `Expr`.
Copyright : (c) Cheuk Ho Yun @2018
License : WTFPL
Maintainer : yunc5@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprParser (pInt, pIntG, pInteger, pIntegerG, pF, pFG, pD, pDG, setInteger, setIntegerG, setInt, setIntG, setF, setFG, setD, setDG) where


import           ExprType

import           Text.Parsec
import           Text.Parsec.String

{- User-end Parsing Functions -}
{- These are the user-end parser functions that take in a string and return the corresponding Expr, the number type will be determined by the parser you choose.-}

-- | Takes in a `String` with (+), (*) and/or (^), return the corresponding `Expr`. The result would be `Expr` Integer.
pInteger :: String -- ^ Take in a `String`
              -> Expr Integer -- ^ Return corresponding `Expr` Integer
pInteger ss = case parse setInteger  "" ss of
                Left err  -> error "This is invalid input for parsing integers"
                Right expr -> expr
-- | Takes in a `String` with "cos", "sin", "log" and "e", return the corresponding `Expr`. The result would be `Expr` Integer.
pIntegerG :: String -- ^ Take in a `String`
               -> Expr Integer -- ^ Return corresponding `Expr` Integer
pIntegerG ss = case parse setIntegerG "" ss of
                Left err  -> error "This is invalid input for parsing variable strings"
                Right expr -> expr

-- | Takes in a `String` with (+), (*) and/or (^), return the corresponding `Expr`. The result would be `Expr` Int.
pInt :: String -- ^ Take in a `String`
                -> Expr Int -- ^ Return corresponding `Expr` Int
pInt ss = case parse setInt  "" ss of
                 Left err  -> error "This is invalid input for parsing integers"
                 Right expr -> expr
-- | Takes in a `String` with "cos", "sin", "log" and "e", return the corresponding `Expr`. The result would be `Expr` Int.
pIntG :: String -- ^ Take in a `String`
              -> Expr Int -- ^ Return corresponding `Expr` Int
pIntG ss = case parse setIntG "" ss of
             Left err  -> error "This is invalid input for parsing variable strings"
             Right expr -> expr

-- | Takes in a `String` with (+), (*) and/or (^), return the corresponding `Expr`. The result would be `Expr` Float.
pF :: String -- ^ Take in a `String`
           -> Expr Float -- ^ Return corresponding `Expr` Float
pF ss = case parse setF  "" ss of
             Left err  -> error "This is invalid input for parsing floats"
             Right expr -> expr
-- | Takes in a `String` with "cos", "sin", "log" and "e", return the corresponding `Expr`. The result would be `Expr` Float.
pFG :: String -- ^ Take in a `String`
            -> Expr Float -- ^ Return corresponding `Expr` Float
pFG ss = case parse setFG "" ss of
             Left err  -> error "This is invalid input for parsing variable strings"
             Right expr -> expr

-- | Takes in a `String` with (+), (*) and/or (^), return the corresponding `Expr`. The result would be `Expr` Double.
pD :: String -- ^ Take in a `String`
        -> Expr Double -- ^ Return corresponding `Expr` Double
pD ss = case parse setD  "" ss of
          Left err  -> error "This is invalid input for parsing doubles"
          Right expr -> expr
-- | Takes in a `String` with "cos", "sin", "log" and "e", return the corresponding `Expr`. The result would be `Expr` Double.
pDG :: String -- ^ Take in a `String`
            -> Expr Double -- ^ Return corresponding `Expr` Double
pDG ss = case parse setDG "" ss of
             Left err  -> error "This is invalid input for parsing variable strings"
             Right expr -> expr




{- The Main Parsers -}
{- These are the main parsers which combines other parsers and perform parsing to different types. -}
-- | Performs parsing and returns `Parser` `Expr` Integer. Checks if any character is an operator (+,- or ^), or if it is a number/variable.
setInteger :: Parser (Expr Integer)
setInteger = (termInteger `chainl1` setOpMain)
-- | Performs parsing and returns `Parser` `Expr` Integer. Checks if any character is an operator (sin, cos, log, e^), or if it is a number/variable.
setIntegerG :: Parser (Expr Integer)
setIntegerG = let
            opRest = do {op <- setOpRest;
                                    spaces;
                                    term <- termIntegerG;
                                    spaces;
                                    return (op term)}
                in try opRest <|> termIntegerG
-- | Performs parsing and returns `Parser` `Expr` Int. Checks if any character is an operator (+,- or ^), or if it is a number/variable.
setInt :: Parser (Expr Int)
setInt = (termInt `chainl1` setOpMain)
-- | Performs parsing and returns `Parser` `Expr` Int. Checks if any character is an operator (sin, cos, log, e^), or if it is a number/variable.
setIntG :: Parser (Expr Int)
setIntG = let
            opRest = do {op <- setOpRest;
                                    spaces;
                                    term <- termIntG;
                                    spaces;
                                    return (op term)}
                in try opRest <|> termIntG
-- | Performs parsing and returns `Parser` `Expr` Float. Checks if any character is an operator (+,- or ^), or if it is a number/variable.
setF :: Parser (Expr Float)
setF = termF `chainl1` setOpMain
-- | Performs parsing and returns `Parser` `Expr` Float. Checks if any character is an operator (sin, cos, log, e^), or if it is a number/variable.
setFG :: Parser (Expr Float)
setFG = let
            opRest = do {op <- setOpRest;
                                    spaces;
                                    term <- termFG;
                                    spaces;
                                    return (op term)}
                in try opRest <|> termFG
-- | Performs parsing and returns `Parser` `Expr` Double. Checks if any character is an operator (+,- or ^), or if it is a number/variable.
setD :: Parser (Expr Double)
setD = termD `chainl1` setOpMain
-- | Performs parsing and returns `Parser` `Expr` Double. Checks if any character is an operator (sin, cos, log, e^), or if it is a number/variable.
setDG :: Parser (Expr Double)
setDG = let
            opRest = do {op <- setOpRest;
                                    spaces;
                                    term <- termDG;
                                    spaces;
                                    return (op term)}
                in try opRest <|> termDG

{- Factors -}
{- Try to parse character into numbers (using numParse), or variables (using varParse). -}
factorInteger :: Parser (Expr Integer)
factorInteger = try numParseInteger <|> varParse -- Try to parse it as a number or operator first, if that doesn't work, parse it as a variable.


factorIntegerG :: Parser (Expr Integer)
factorIntegerG = try numParseInteger <|> varParse

factorInt :: Parser (Expr Int)
factorInt = try numParseInt <|> varParse

factorIntG :: Parser (Expr Int)
factorIntG = try numParseInt <|> varParse

factorF :: Parser (Expr Float)
factorF = try numParseF <|> varParse

factorFG :: Parser (Expr Float)
factorFG = try numParseF <|> varParse

factorD :: Parser (Expr Double)
factorD = try numParseD <|> varParse

factorDG :: Parser (Expr Double)
factorDG = try numParseD <|> varParse


{- "Term"s -}
{- Call `factor`s, and set the sign for the character parsed, with the help of `notOP` to determine whether it should be positive or negative.-}
termInteger :: Parser (Expr Integer)
termInteger = (notOp factorInteger) <|> factorInteger -- Call factor and parse the character into number or variable, then determine its sign with notOP.

termIntegerG :: Parser (Expr Integer)
termIntegerG = (notOp factorIntegerG) <|> factorIntegerG

termInt :: Parser (Expr Int)
termInt = (notOp factorInt) <|> factorInt

termIntG :: Parser (Expr Int)
termIntG = (notOp factorIntG) <|> factorIntG

termF :: Parser (Expr Float)
termF = (notOp factorF) <|> factorF

termFG :: Parser (Expr Float)
termFG = (notOp factorFG) <|> factorFG

termD :: Parser (Expr Double)
termD = (notOp factorD) <|> factorD

termDG :: Parser (Expr Double)
termDG = (notOp factorDG) <|> factorDG

{- Num Parsers -}
{- Parses numbers -}
numParseInteger :: Parser (Expr Integer)
numParseInteger = do {i <- integer;
               return (Const i)}

numParseInt :: Parser (Expr Int)
numParseInt = do {i <- int;
               return (Const i)}

numParseF :: Parser (Expr Float)
numParseF = do {i <- float;
           return (Const i)}

numParseD :: Parser (Expr Double)
numParseD = do {i <- double;
                return (Const i)}


{- Auxiliary Functions -}
varParse :: Parser (Expr a) -- When sees a string that cannot be parsed as operator or numbers, call this parser to parse it into a variable.
varParse = do { s <- many1 letter;
                return (Var s)}

setOpMain :: Parser (Expr a -> Expr a-> Expr a) -- Parses main operators which take two arguments.
setOpMain = (do { symbol "+"; return Add })
     <|> do { symbol "*"; return Mult }
     <|> do { symbol "^"; return Pow}

setOpRest :: Parser (Expr a -> Expr a) -- Parses other operators that take 1 argument.
setOpRest = (do { string "e"; return Exp })
    <|> do { string "Log"; return Log }
    <|> do { string "cos"; return Cos}
    <|> do { string "sin"; return Sin}

notOp :: (Num a) => Parser (Expr a) -- Look for negative sign, and times what follows with negative one.
                    -> Parser (Expr a)
notOp p = do { symbol "-" ;
               exp <- p ;
               return $ Mult (Const (-1)) exp }

symbol :: String
          -> Parser String
symbol ss = let
    symbol' :: Parser String
    symbol' = do { ss' <- string ss;
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

int :: Parser Int
int = fmap read $ try negDigits <|> digits

doubleDigits :: Parser String
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }

decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }

double :: Parser Double
double = fmap read $ doubleDigits

float :: Parser Float
float = fmap read $ doubleDigits
