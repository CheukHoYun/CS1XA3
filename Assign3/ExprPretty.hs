{-|
Module : ExprPretty
Description : The module used to define what the `Expr` looks like when it is printed on screen. Makes the output readable.
Copyright : (c) Cheuk Ho Yun @2018
License : WTFPL
Maintainer : yunc5@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprPretty where

import           ExprType

-- | A function defined by Curtis, used to add brackets out of a certain string. Surprisingly useful.
parens :: String
          -> String
parens ss = "(" ++ ss ++ ")"

instance (Show a, Num a, Ord a) => Show (Expr a) where
  show (Mult e1 (Const a)) = parens $ (show a) ++ " * " ++ (show e1)
  show (Mult e1 e2) = parens $ (show e1) ++ " * " ++ (show e2)
  show (Add e1 (Const a)) = case (a<0) of
                            True -> parens $ (show e1) ++ " - " ++ (show (-a))
                            False -> parens $ (show e1) ++ " + " ++ (show a)
  show (Add e1 e2)  = parens $ (show e1) ++ " + " ++ (show e2)
  show (Var ss)     = ss
  show (Const x)    = show x
  show (Cos e1)     = parens $ "cos(" ++ show e1 ++ ")"
  show (Sin e1)     = parens $ "sin(" ++ show e1 ++ ")"
  show (Pow x n)    = parens $ (show x) ++ "^" ++ (show n)
  show (Log e1)     = parens $ "ln(" ++ (show e1) ++ ")"
  show (Exp e1)     = parens $ "e^" ++ (parens (show e1))
