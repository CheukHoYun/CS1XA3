{-|
Module : ExprType
Description : The module that introduces the datatypes used in the program.
Copyright : (c) Cheuk Ho Yun @2018
License : WTFPL
Maintainer : yunc5@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprType where

import           Data.List
import qualified Data.Map.Strict as Map

-- * DataType Declaration
data Expr a = Add (Expr a) (Expr a) -- ^ Binary Addition
            | Mult (Expr a) (Expr a) -- ^ Binary Multiplication
            | Const a -- ^ Value Wrapper
            | Var String -- ^ Variable Wrapper
            | Cos (Expr a) -- ^ Cosine operation
            | Sin (Expr a) -- ^ Sine operation
            | Log (Expr a) -- ^ Ln operation
            | Exp (Expr a) -- ^ e^x
            | Pow (Expr a) (Expr a) -- ^ An expression to power of another expression
  deriving (Ord)

instance (Eq a) => Eq (Expr a) where
    Add a b == Add c d = ((a==c)&&(b==d)) || ((a==d)&&(b==c))
    Mult a b == Mult c d = ((a==c)&&(b==d)) || ((a==d)&&(b==c))
    Const a == Const b = a==b
    Var a == Var b = a==b
    Cos a == Cos b = a==b
    Sin a == Sin b = a==b
    Log a == Log b = a==b
    Exp a == Exp b = a==b
    Pow a b == Pow c d = (a==c)&&(b==d)
    _ == _ = False

-- * Utility Functions
-- | Grab the variables out from an `Expr`, make them a list
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]
getVars (Cos e) = getVars e
getVars (Sin e) = getVars e
getVars (Log e) = getVars e
getVars (Exp e) = getVars e
getVars (Pow x a) = getVars x `union` getVars a
-- | An function used to add two expressions into a new expression. Only used in test cases.
(!+) :: (Num a ) => Expr a -> Expr a -> Expr a
e1 !+ e2 =  Add e1 e2
-- | An function used to multiply two expressions into a new expression. Only used in test cases.
(!*) :: (Num a ) => Expr a -> Expr a -> Expr a
e1 !* e2 =  Mult e1 e2
-- | An function used to subtract one expression from another, retuns a new expression. Only used in test cases.
(!-) :: (Num a ) => Expr a -> Expr a -> Expr a
e1 !- e2 =  Mult e1 (Mult (Const (-1)) e2)
-- | An function used to devide one expression from another, retuns a new expression. Only used in test cases.
(!/) :: (Num a ) => Expr a -> Expr a -> Expr a
e1 !/ e2 =  Mult e1 (Pow e2 (Const (-1)))
-- | An auxiliary function used to determine if a giving string is in the given variable list. Returns a `Bool`.
isThere :: Map.Map String a -> String -> Bool
isThere vrs x = case Map.lookup x vrs of
                   Just a -> True
                   Nothing -> False
-- | An auxiliary function used to determine if ALL given strings are in the given variable list. Returns a `Bool`.
areThere :: Map.Map String a -> [String] -> Bool
areThere vrs [] = True
areThere vrs (x:xs) =  (isThere vrs x) && (areThere vrs xs)
