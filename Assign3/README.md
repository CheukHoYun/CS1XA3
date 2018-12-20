# Assignment 3 #

## Overview ##
This is a math library with an expression datatype that can encode addition, multiplication, cos, sin, log, exp, variables and constants. It also provides simple parsing features for such expressions. 

## Docs Link ##
https://yunc5.github.io/CS1XA3/

## Functions ##
1. The program can evaluate an expression with variable names given in map. Returns an error if any variable cannot be found. (`eval`)
2. The program can simplify the expression to a natural, hand-written form. (`simplify`)
3. The program can perform partial differentiation on the expression, with the variable name given in `String`. (`partDiff`)
4. The program can take in some string and try to turn it into an expression. The expression must:
-   Be made up with numbers, variables and operators. Operators can only be either (+,-,^) or (cos, sin, e^, ln).
-   Not containing any spaces or brackets.

## What's Special ##
1. It actually simplifies the expression! The expression will look exactly like what you usually write. You won't see anything like (x^2+x^2), (x*x^2), (0*x), (x*1), (1+x)*(x+5).. Or ANYTHING not simplified thoroughly! Basically, all expressions will be expanded and calculated until they are actually in the simpliest form.
2. It can do partial differentiation flawlessly. No matter how many variables are inside the expression, you always get what you should get. After some effort, it can now work on the expression even if you have x on both base and exponent. The result may look messy, but it works!

## Known Issue ##
1. The parser does not accept spaces or parens, and it will not process * before +. That is to say, something like a+b*c will be parsed as (Mult (Add (a) (b)) (c)), which isn't what it should look like.
2. It may take quite a while (or forever) if trying to simplify high-power expressions, because the program uses a recursive definition to break all exponential expressions into multiples.


## Thanks to: ##
- Noa Barsky (https://github.com/barskyn) The test cases are grabbed from her code, and the idea of parsing is based on her work.
- https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
- https://stackoverflow.com/questions/20807795/haskell-conflicting-definitions-when-instancing/20807908
- https://stackoverflow.com/questions/45763535/pattern-matching-redundant
- https://stackoverflow.com/questions/15317895/correct-syntax-for-if-statements-in-haskell
- https://stackoverflow.com/questions/324311/symbolic-simplification-in-haskell-using-recursion
- https://stackoverflow.com/questions/38917713/haskellcould-not-deduce-floating-t-arising-from-a-use-of-cos
