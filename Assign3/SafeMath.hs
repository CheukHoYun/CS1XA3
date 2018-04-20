{-|
Module : SafeMath
Description : A simple module used to adapt prelude math functions to any type, instead of just `Floating`.
Copyright : (c) Cheuk Ho Yun @2018
License : WTFPL
Maintainer : yunc5@mcmaster.ca
Stability : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module SafeMath where


class SafeMath a where
    -- | Adapt the prelude cos function to all number types
    myCos :: a -> a
    -- | Adapt the prelude sin function to all number types
    mySin :: a -> a
    -- | Adapt the prelude log function to all number types
    myLog :: a -> a
    -- | Adapt the prelude exp function to all number types
    myExp :: a -> a
    -- | Adapt the prelude (**) function to all number types
    myPow :: a -> a -> a



instance SafeMath Float where
    myCos n = cos n
    mySin n = sin n
    myLog n = log n
    myExp n = exp n
    myPow x a = x ** a



instance SafeMath Double where
    myCos n = cos n
    mySin n = sin n
    myLog n = log n
    myExp n = exp n
    myPow x a = x ** a

instance SafeMath Integer where
    myCos n = round $ cos (fromIntegral n)
    mySin n = round $ sin (fromIntegral n)
    myLog n = round $ log (fromIntegral n)
    myExp n = round $ exp (fromIntegral n)
    myPow x a = round $ (fromIntegral x) ** (fromIntegral a)

instance SafeMath Int where
    myCos n = round $ cos (fromIntegral n)
    mySin n = round $ sin (fromIntegral n)
    myLog n = round $ log (fromIntegral n)
    myExp n = round $ exp (fromIntegral n)
    myPow x a = round $ (fromIntegral x) ** (fromIntegral a)
