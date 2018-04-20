{-|
Module : ExprDiff
Description : The module used to eavluate, simplify and do partial differentiation to expressions.
Copyright : (c) Cheuk Ho Yun @2018
License : WTFPL
Maintainer : yunc5@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff (eval, simplify, partDiff, diff, simplifiable, simplifyAdd, simplifyMult, simplifyPow) where

import           ExprType
import           SafeMath

import qualified Data.Map.Strict as Map

-- * Main Operations
class (Eq a, Ord a, Num a) => DiffExpr a where

  -- | Get the value of expression. Return error if not enough information is given.
  eval :: Map.Map String a -- ^ Takes in a variable dictionary
          -> Expr a -- ^ Takes in an expression
          -> a -- ^ Return its value, if possible

  -- | Simplify the expression to minimal form.
  simplify :: Map.Map String a -- ^ Takes in a variable dictionary
              -> Expr a -- ^ Takes in an expression
              -> Expr a -- ^ Return the simplified form of expression

  -- | Perform partial differentiation to an expression with a given variable name.
  partDiff :: String -- ^ Takes in the variable to be partially derived for, as a string
              -> Expr a -- ^ Takes in the expression to be derived from
              -> Expr a -- ^ Return the partially derived form of the expression

  -- | Was designed as an auxiliary function for `partDiff`, but it can actually perform differentiation based on ANY expression, not just a single variable. The `partDiff` function is actually a special application of `diff`.
  diff :: Expr a -- ^ Takes in the expression you want to derive for
          -> Expr a -- ^ Takes in the expression you want to derive from
          -> Expr a -- ^ Return the derivative of the second expression for the first expression

  -- | A function used to determine if an expression can be simplified or not, returns a `Bool`.
  simplifiable :: Map.Map String a -- ^ Takes in a variable dictionary
                  -> Expr a -- ^ Takes in an expression
                  -> Bool -- ^ Return `True` if it can be simplified. Return `False` if it cannot.
  simplifiable vrs e1 = ((simplify vrs e1) /= e1) -- Is expression the same as it was when it's simplified? If so, it's not simplifiable.

  -- | Auxiliary function used to simplify specifically Add types, and perform thorough simplification based on commutative laws.
  simplifyAdd :: Map.Map String a -- ^ Takes in a variable dictionary
                 -> Expr a -- ^ Takes in an expression. Must be in Add form.
                 -> Expr a -- ^ Apply special tricks for simplifying Add types.

  -- | Auxiliary function used to simplify specifically Mult types, and perform thorough simplification based on commutative laws and associative laws.
  simplifyMult :: Map.Map String a -- ^ Takes in a variable dictionary
                  -> Expr a -- ^ Takes in an expression. Must be in Mult form.
                  -> Expr a -- ^ Apply special tricks for simplifying Mult types.

  -- | Auxiliary function used to simplify specifically Pow types.
  simplifyPow :: Map.Map String a -- ^ Takes in a variable dictionary
                 -> Expr a -- ^ Takes in an expression. Must be in Pow form.
                 -> Expr a -- ^ Apply special tricks for simplifying Pow types.



instance (Num a, Eq a, Ord a, SafeMath a) => DiffExpr a where

{- Evaluates the Expr-}
    eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
    eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
    eval vrs (Const x) = x
    eval vrs (Var x) = case Map.lookup x vrs of
                       Just a  -> a
                       Nothing -> error ("The expression includes a variable called \"" ++ x ++ "\", whose value is unknown. Evaluation can't be done.")
    eval vrs (Cos x) = myCos (eval vrs x)
    eval vrs (Sin x) = mySin (eval vrs x)
    eval vrs (Log x) = myLog (eval vrs x)
    eval vrs (Exp x) = myExp (eval vrs x)
    eval vrs (Pow x n) = myPow (eval vrs x) (eval vrs n)


{-The three simplify patterns which works for Add, Mult and Pow types.
  The logic here is simple: Check if the sub-expressions are simplifiable,
  if they are, simplify them first and check again. If they are not, then
  apply the special simplification tricks for Add, Mult, Pow types.
  (Namely `simplifyAdd`, `simplifyMult`, `simplifyPow`)
-}
    simplify vrs (Add e1 e2) = case ((simplifiable vrs e1)||(simplifiable vrs e2)) of
                                True -> (simplify vrs (Add (simplify vrs e1) (simplify vrs e2)))
                                False -> (simplifyAdd vrs (Add e1 e2))
    simplify vrs (Mult e1 e2) = case ((simplifiable vrs e1)||(simplifiable vrs e2)) of
                                True -> (simplify vrs (Mult (simplify vrs e1) (simplify vrs e2)))
                                False -> (simplifyMult vrs (Mult e1 e2))
    simplify vrs (Pow e1 n) = case ((simplifiable vrs e1) || (simplifiable vrs n)) of
                              True -> simplify vrs (Pow (simplify vrs e1) (simplify vrs n))
                              False -> (simplifyPow vrs (Pow e1 n))

{- Basic simplify cases.
   These are the simplify method for the types who
   only take in one argument. If the variables passed
   into them by their following expressions are given in the dictionary,
   return their value and wrap them up as `Const`,
   otherwise, return themselves but simplify the expression passed in.
   -}
    simplify vrs (Const a) = (Const a)

    simplify vrs (Var x) = case Map.lookup x vrs of
                           Just a -> (Const a)
                           Nothing -> (Var x)

    simplify vrs (Cos e1) = case (areThere vrs (getVars e1)) of
                            True -> (Const (eval vrs (Cos e1)))
                            False -> (Cos (simplify vrs e1))
    simplify vrs (Sin e1) = case (areThere vrs (getVars e1)) of
                            True -> (Const (eval vrs (Sin e1)))
                            False -> (Sin (simplify vrs e1))
    simplify vrs (Log e1) = case (areThere vrs (getVars e1)) of
                            True -> (Const (eval vrs (Log e1)))
                            False -> (Log (simplify vrs e1))
    simplify vrs (Exp e1) = case (areThere vrs (getVars e1)) of
                            True -> (Const (eval vrs (Exp e1)))
                            False -> (Exp (simplify vrs e1))

{- Special simplify methods for Pow. -}
    simplifyPow vrs (Pow (Const 0) (Const a))
        | (a>0) = Const 0
        | otherwise = error "0's negative power is undefined."
    simplifyPow vrs (Pow (Const a) (Const b)) = (Const (eval vrs (Pow (Const a) (Const b))))               -- a^b where a, b are numbers. Calculate the result and wrap it as a constant.
    simplifyPow vrs (Pow e1 (Const 0)) = Const 1
    simplifyPow vrs (Pow e1 (Const 1)) = e1                                                                -- Anything with power 1 should be itself.
    simplifyPow vrs (Pow (Var x) n) = (Pow (Var x) n)                                                      -- Keep the power sign and don't simplify forms like x^a. It looks better than x*x*x...
    simplifyPow vrs (Pow e1 (Const n)) = simplify vrs (Mult (simplifyPow vrs (Pow e1 (Const (n-1)))) e1)   -- Some expression that is not a variable to power `a` should be expanded. E.g. (cos(x)+2y+z^3)^2 = (cos(x)+2y+z^3) * (cos(x)+2y+z^3), and then it will be simplified by Mult rules. (See below)
    simplifyPow vrs (Pow e1 e2) = (Pow e1 e2)                                                              -- Last resort: If we can't apply any of the above rules, return what it is.

{- Special simplify methods for Add. -}
    simplifyAdd vrs (Add (Const a) (Const b)) =  (Const (a+b))                                             -- (a+b) where a, b are numbers, Calculate the result and wrap it as a constant.
    simplifyAdd vrs (Add (Const 0) e1) = e1                                                                -- simplify anything with form (0+x) to x. We don't want redundant 0s in simplified form.
    simplifyAdd vrs (Add e1 (Const 0)) = e1                                                                -- Same as above.
    simplifyAdd vrs (Add (Mult (Const a) (e1)) (Mult (Const b) (e2))) = case (e1==e2) of               -- Like item merger. ax+bx = (a+b)x. Here x is an expression and its coefficient isn't 1.
                                                                           True -> Mult (Const (a+b)) (e1)
                                                                           False -> (Add (Mult (Const a) (e1)) (Mult (Const b) (e2)))
    simplifyAdd vrs (Add (Mult (Const a) (e1)) (Mult (e2) (Const b))) = case (e1==e2) of               -- Same as above.
                                                                           True -> Mult (Const (a+b)) (e1)
                                                                           False -> (Add (Mult (Const a) (e1)) (Mult (e2) (Const b)))
    simplifyAdd vrs (Add (Mult (e1) (Const a)) (Mult (Const b) (e2))) = case (e1==e2) of               -- Same as above.
                                                                           True -> Mult (Const (a+b)) (e1)
                                                                           False -> (Add (Mult (e1) (Const a)) (Mult (Const b) (e2)))
    simplifyAdd vrs (Add (Mult (e1) (Const a)) (Mult (e2) (Const b))) = case (e1==e2) of               -- Same as above.
                                                                           True -> Mult (Const (a+b)) (e1)
                                                                           False -> (Add (Mult (e1) (Const a)) (Mult (e2) (Const b)))

    simplifyAdd vrs (Add (Mult (Const a) (e1)) (e2)) = case (e1==e2) of
                                                        True -> Mult (Const (a+1)) e1
                                                        False -> (Add (Mult (Const a) (e1) ) (e2))
    simplifyAdd vrs (Add (e2) (Mult (Const a) (e1))) = case (e1==e2) of
                                                        True -> Mult (Const (a+1)) e1
                                                        False -> (Add (e2) (Mult (Const a) (e1)))
    simplifyAdd vrs (Add e1 (Add e2 e3)) = if (simplifiable vrs (Add e1 e2)) then (simplify vrs (Add (Add e1 e2) e3))           -- Using commutative property, simplify thoroughly while adding an Add form expression to another expression. Logic: when doing (a) + (b + c), though b and c may not be simplified while adding, (a + b) or (a + c) may be simplified. Go through them to check.
                                           else if (simplifiable vrs (Add e1 e3)) then (simplify vrs (Add (Add e1 e3) e2))
                                           else (Add e1 (Add e2 e3))
    simplifyAdd vrs (Add (Add e1 e2) e3) = if (simplifiable vrs (Add e1 e3)) then (simplify vrs (Add (Add e1 e3) e2))           -- Same as above, order reversed.
                                           else if (simplifiable vrs (Add e2 e3)) then (simplify vrs (Add (Add e2 e3) e1))
                                           else (Add (Add e1 e2) e3)
    simplifyAdd vrs (Add e1 e2) = case (e1==e2) of                                                         -- Last resort: if none of the rules above can be applied, see if the addends are the same. If they are, time it by two; if they aren't, return the original expression.
                                  True -> (Mult (Const 2) e1)
                                  False -> (Add e1 e2)

{- Special simplify methods for Mult type. -}
    simplifyMult vrs (Mult e1 (Const 1)) = e1                                                              -- Anything times one should be itself. Cuts redundant 1s.
    simplifyMult vrs (Mult (Const 1) e1) = e1                                                              -- Same as above.
    simplifyMult vrs (Mult e1 (Const 0)) = Const 0                                                         -- Anything times zero should be zero. Cuts redundant expressions.
    simplifyMult vrs (Mult (Const 0) e1) = Const 0                                                         -- Same as above.
    simplifyMult vrs (Mult (Const a) (Const b)) = (Const (a*b))                                            -- Multiplying 2 constants should give you a number. Wrap it up as another constant.
    simplifyMult vrs (Mult (Var x) (Var y)) = case (x==y) of                                               -- Multiplying two variables: if they are the same variable, turn it into second power. If they're not, do nothing.
                                              True -> (Pow (Var x) (Const 2))
                                              False -> (Mult (Var x) (Var y))
    simplifyMult vrs (Mult (Var x) (Pow (Var y) a)) = case (x==y) of
                                                      True -> simplify vrs (Pow (Var y) (Add (Const 1) (a)))
                                                      False -> (Mult (Var x) (Pow (Var y) a))
    simplifyMult vrs (Mult (Pow (Var y) a) (Var x)) = case (x==y) of
                                                      True -> simplify vrs (Pow (Var y) (Add (Const 1) (a)))
                                                      False -> (Mult (Var x) (Pow (Var y) a))
    simplifyMult vrs (Mult (Pow x1 n1) (Pow x2 n2)) = case (x1==x2) of                                     -- If there are two same power expressions (A Pow expression must have power >=2), combine them and make it a higher power statement. It will be simplified using the Pow rules above.
                                                        True -> simplify vrs (Pow x1 (Add n1 n2))
                                                        False -> (Mult (Pow x1 n1) (Pow x2 n2))
    simplifyMult vrs (Mult e1 (Mult e2 e3)) = if (simplifiable vrs (Mult e1 e2)) then (simplify vrs (Mult (Mult e1 e2) e3)) -- Using commutative property to thoroughly simplify expressions. Logic same as commutative property in Add. See above.
                                           else if (simplifiable vrs (Mult e1 e3)) then (simplify vrs (Mult (Mult e1 e3) e2))
                                           else (Mult e1 (Mult e2 e3))
    simplifyMult vrs (Mult (Mult e1 e2) e3) = if (simplifiable vrs (Mult e1 e3)) then (simplify vrs (Mult (Mult e1 e3) e2))
                                           else if (simplifiable vrs (Mult e2 e3)) then (simplify vrs (Mult (Mult e2 e3) e1))
                                           else (Mult (Mult e1 e2) e3)
    simplifyMult vrs (Mult e1 (Add e2 e3)) = simplify vrs (Add (Mult e1 e2) (Mult e1 e3))                  -- Using associative property to simplify thoroughly. Logic: when doing a * ( b + c ), although a* (b + c) may not be simplified anymore, (a * b) or (a * c) may be simplifiable. Expand the expression and check, if no other Mult simplify rules can be applied.
    simplifyMult vrs (Mult (Add e1 e2) e3) = simplify vrs (Add (Mult e1 e3) (Mult e2 e3))                  -- Same as above.
    simplifyMult vrs (Mult e1 e2) = (Mult e1 e2)                                                           -- Last resort: If not rules above can be applied, return what it is.

{- Differential Methods -}
    diff e (Const a) = Const 0                                                                             -- Derive anything from a constant should return zero.
    -- diff e (Mult (Const a) e1) = Mult (Const a) (diff e e1)                                                -- When deriving something from some expression with a coefficient, the coefficient can be picked out. No longer needed because covered by a more general product rule. See below.
    -- diff e (Mult e1 (Const a)) = Mult (Const a) (diff e e1)                                                -- Same as above.
    diff e (Add e1 e2) = Add (diff e e1) (diff e e2)                                                       -- Sum rule.
    diff e (Mult e1 e2) = Add (Mult (diff e e1) e2) (Mult e1 (diff e e2))                                  -- Product rule.
    diff e (Sin e1) = Mult (Cos e1) (diff e e1)                                                            -- sin(x)' = cos(x). Chain rule applied.
    diff e (Cos e1) = Mult (Mult (Const (-1)) (Cos e1)) (diff e e1)                                        -- cos(x)' = -sin(x). Chain rule applied.
    diff e (Exp e1) = Mult (Exp e1) (diff e e1)                                                            -- (e^x)' = (e^x). Chain rule applied.
    diff e (Log e1) = Mult (Pow e1 (Const (-1))) (diff e e1)                                               -- ln(x)' = 1/x. Chain rule applied.
    diff e (Pow e1 e2) = Add (Mult (Mult (e2) (Pow e1 (Add e2 (Const (-1))))) (diff e e1)) (Mult (Mult (Log e1) (Pow e1 e2)) (diff e e2)) -- Two rules used here: #1 If in x^a form, dy/dx = a*x^(a-1) #2 If in a^x form, dy/dx = (a^x)*ln(a). We are unable to tell which of e1 and e2 contains a variable, but in either #1 or #2 form, the other result will be zero. Therefore, adding the two results up will always get us the right result. Even if you have x on both base and exponent, it will work. I don't know the math behind, it just happened to work.
    diff e1 e2 = case (e1==e2) of                                                                          -- Finally, if e1 and e2 are same expressions, should return 1.
                 True -> (Const 1)
                 False -> (Const 0)

    partDiff x e = simplify (Map.fromList []) (diff (Var x) e)                                             -- Pass x as the variable name, do the differentiation!
