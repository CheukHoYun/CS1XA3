module Haskell01 where

-- #TODO: put your macid in the following string
macid = "yunc5"

{- ------------------------------------------------------------------------------------------------------------
 - Part 1: Either
 - ------------------------------------------------------------------------------------------------------------
 -}
data MyEither a b = MyLeft a | MyRight b
  deriving (Show, Eq)

instance Functor (MyEither a) where
  fmap f (MyRight x) = MyRight $ f x
  fmap _ (MyLeft x) = MyLeft x


instance Applicative (MyEither a) where
  pure val = MyRight val
  MyRight f <*> x = fmap f x
  MyLeft y  <*> _ = MyLeft y

{- Task 1:                          (1 Mark)
 -     Complete the following Monad Instance
 -     Make sure it obeys the following laws
 - ------------------------------------------------------------------------------------------------------------
 -       Left Identity:  return a >>= f   ==  f a
 -       Right Identity: m >>= return     ==  m
 -       Associative:    (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
 - ------------------------------------------------------------------------------------------------------------
 -}
instance Monad (MyEither a) where
    return val = MyRight val
    MyRight x >>= f = f x
    MyLeft x >>= f = MyLeft x
  -- #TODO: Implement (>>=) :: MyEither a b -> (b -> MyEither a c) -> MyEither a c

{- ------------------------------------------------------------------------------------------------------------
 - Part 2: List
 - ------------------------------------------------------------------------------------------------------------
 -}
data List a = Cons a (List a) | Empty
  deriving (Show,Eq)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Empty       = Empty

{- Task 2:                            (1 Mark)
 -     Complete the following Monoid Instance
 -     Make sure it obeys the following laws
 - ------------------------------------------------------------------------------------------------------------
 -       Left Identity:  mempty `mappend` x  == x
 -       Right Identity: x `mappend` mempty  == x
 -       Associative:    (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)
 - ------------------------------------------------------------------------------------------------------------
 -     Hint: mappend corresponds to the (++) function on built-in Haskell lists
 -}

cat :: List a -> List a -> List a
cat Empty ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

instance Monoid (List a) where
  mempty = Empty
  mappend = cat
  -- #TODO: Implement mappend :: List a -> List a -> List a

{- Task 3:                                 (1 Mark)
 -     Complete the following Applicative Instance
 -     Make sure it obeys the following laws
 - ------------------------------------------------------------------------------------------------------------
 -       Identity:       pure id <*> xs    == xs
 -       Homomorphism:   pure f <*> pure x == pure (f x)
 - ------------------------------------------------------------------------------------------------------------
 -     Hint: [f1,...fn] <*> [x1,...,xi] == f1 x1 `mappend` ... f1 xi `mappend` ... fn x1 `mappend` ... fn xi
 -}
instance Applicative List where
  pure x = Cons x Empty
  _ <*> Empty = Empty
  Empty <*> _ = Empty
  (Cons f (fs)) <*> xs =  mappend (fmap f xs) (fs <*> xs)
  -- #TODO: Implement <*> :: List (a -> b) -> List a -> List b


{- Task 4:                          (2 Marks)
 -     Complete the following Monad Instance
 -     Make sure it obeys the following laws
 - ------------------------------------------------------------------------------------------------------------
 -       Left Identity:  return a >>= f   ==  f a
 -       Right Identity: m >>= return     ==  m
 -       Associative:    (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
 - ------------------------------------------------------------------------------------------------------------
 -     Hint: [x1,..,xn] >>= f == f x1 `mappend` ... `mappend` f xn
 -}
instance Monad List where
    return val = Cons val Empty
    (Cons x (xs)) >>= f = mappend (f x) (xs >>= f)
    Empty >>= _ = Empty
  -- #TODO: Implement (>>=) :: List a -> (a -> List b) -> List b
