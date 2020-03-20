{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
module Memo
  ( memo
  , Memo(..) ) where

import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics

-- | A helper function which allows you to write functions which don't
-- repeat unnecessary work. The typical usage will involve defining f as
-- memo f' and then using f for the recursive calls in the definition f'.
-- This will populate the tabular representation of the function as you use
-- the function, and subsequent calls to the same index of the table do not
-- result in repopulating it.
memo :: Memo x => (x -> y) -> x -> y
memo = index . tabulate

-- | A class which allows us to transform functions to and from their
-- tabular form.
class Memo k where
  data Table k :: * -> *
  tabulate :: (k -> r) -> Table k r
  index :: Table k r -> k -> r

instance Memo () where
  data Table () a = Spot a
  tabulate f = Spot (f ())
  index (Spot x) _ = x

instance Memo Bool where
  data Table Bool a = BoolTable a a
  tabulate f = BoolTable (f True) (f False)
  index (BoolTable x y) = \case
    True -> x
    False -> y

instance (Memo x, Memo y) => Memo (Either x y) where
  data Table (Either x y) a = EitherTable (Table x a) (Table y a)
  tabulate f = EitherTable (tabulate (f . Left)) (tabulate (f . Right))
  index (EitherTable l r) = either (index l) (index r)

instance (Memo x, Memo y) => Memo (x, y) where
  newtype Table (x, y) a = PairTable (Table x (Table y a))
  tabulate f = PairTable (tabulate \x -> tabulate \y -> f (x, y))
  index (PairTable p) (x, y) = index (index p x) y

instance Memo x => Memo [x] where
  data Table [x] a = ListTable a (Table x (Table [x] a))
  tabulate f = ListTable (f []) (tabulate \x -> tabulate \xs -> f (x : xs))
  index (ListTable a as) = \case
    [] -> a
    (x : xs) -> index (index as x) xs

instance Memo x => Memo (NonEmpty x) where
  newtype Table (NonEmpty x) a = NonEmptyTable (Table x (Table [x] a))
  tabulate f = NonEmptyTable (tabulate \x -> tabulate \xs -> f (x :| xs))
  index (NonEmptyTable as) (x :| xs) = index (index as x) xs

instance Memo Integer where
  newtype Table Integer a = IntegerTable (Table (NonEmpty Bool) a)
  tabulate f = IntegerTable (tabulate \bs -> f (integerOf bs)) where
    integerOf (b :| bs) = if b then go bs else -(go bs) where
      go (True : bs) = 1 + 2 * go bs
      go (False : bs) = 2 * go bs
      go [] = 0
  index (IntegerTable t) i = index t (bitsOf i) where
    bitsOf n = if n >= 0 then True :| go n else False :| go (- n) where
      go n
        | n <= 0 = []
        | n `mod` 2 == 1 = True : go (n `div` 2)
        | otherwise = False : go (n `div` 2)

instance Memo x => Memo (Maybe x) where
  data Table (Maybe x) a = MaybeTable a (Table x a)
  tabulate f = MaybeTable (f Nothing) (tabulate (f . Just))
  index (MaybeTable a t) = \case
    Nothing -> a
    Just x -> index t x

newtype Fix f = Fix { runFix :: f (Fix f) }

instance (forall x. Memo x => Memo (f x)) => Memo (Fix f) where
  data Table (Fix f) a = FixTable (Table (f (Fix f)) a)
  tabulate f = FixTable (tabulate \x -> f (Fix x))
  index (FixTable e) = index e . runFix

instance (Memo (f x), Memo (g x)) => Memo ((f :+: g) x) where
  data Table ((f :+: g) x) a = SumTable (Table (Either (f x) (g x)) a)
  tabulate f = SumTable (tabulate (f . either L1 R1))
  index (SumTable e) = index e . \case
    L1 fx -> Left fx
    R1 gx -> Right gx

instance (Memo (f x), Memo (g x)) => Memo ((f :*: g) x) where
  data Table ((f :*: g) x) a = ProdTable (Table (f x, g x) a)
  tabulate f = ProdTable (tabulate (f . \(fx, gx) -> fx :*: gx))
  index (ProdTable e) = index e . \(fx :*: gx) -> (fx, gx)
