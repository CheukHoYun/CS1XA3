{-|
Module : ExprTest
Description : The module that combines all modules together. The main entrance of the program. Sevral test cases included as well.
Copyright : (c) Cheuk Ho Yun @2018
License : WTFPL
Maintainer : yunc5@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType
import           SafeMath

import qualified Data.Map.Strict as Map
import           Test.QuickCheck


import           Text.Parsec
import           Text.Parsec.String

testvr = Map.fromList [ ("x",5), ("y", 7.8)]
-- * Test Cases

-- | Checks if two variables can be added rightly when given their value.
evalProp1 :: Double -> Double -> Bool
evalProp1 a b = eval (Map.fromList [("x",a),("y",b)]) (Add (Var "x") (Var "y")) == a+b
testevalProp1 = quickCheck evalProp1
-- | Checks if two variables can be multiplied rightly when given their value.
evalProp2 :: Double -> Double -> Bool
evalProp2 a b = eval (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Var "y")) == a*b
testevalProp2 = quickCheck evalProp2
-- | Checks if e^x can be valued rightly.
evalProp3 :: Double -> Bool
evalProp3 a= eval (Map.fromList [("x",a)]) (Exp (Var "x")) == exp(a)
testevalProp3 = quickCheck evalProp3
-- | Checks if e^x can be valued rightly, when redundant vaialbe value is given in the dictionary.
evalProp4 :: Double -> Double -> Bool
evalProp4 a b = eval (Map.fromList [("x",a),("y",b)]) (Exp (Var "x")) == exp(a)
testevalProp4 = quickCheck evalProp4
-- | Checks if the second variable's value is right, when redundant vaialbe value is given earlier in the dictionary.
evalProp5 :: Double -> Double -> Bool
evalProp5 a b = eval (Map.fromList [("x",a),("y",b)]) (Var "y") == b
testevalProp5 = quickCheck evalProp5
-- | Checks if negative constants work alright while being evaluated.
evalProp6 :: Double -> Double -> Bool
evalProp6 a b = eval (Map.fromList [("x",a),("y",b)]) ((!*) (Const (-1)) (Const a)) == -a
testevalProp6 = quickCheck evalProp6
-- | Checks if cos works.
evalProp7 :: Double -> Double -> Bool
evalProp7 a b = eval (Map.fromList [("x",a),("y",b)]) (Cos (Var "x")) == cos (a)
testevalProp7 = quickCheck evalProp7
-- | Checks if sin works.
evalProp8 :: Double -> Double -> Bool
evalProp8 a b = eval (Map.fromList [("x",a),("y",b)]) (Sin (Var "x")) == sin (a)
testevalProp8 = quickCheck evalProp8

-- | Checks if doing partial differentiation to a constant returns a zero.
diffProp1 :: String -> Double -> Bool
diffProp1 s a = partDiff s (Const a) == Const 0
testdiffProp1 = quickCheck diffProp1
