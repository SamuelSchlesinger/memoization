{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
module Memo where

-- Inspired by https://www.youtube.com/watch?v=XtogTwzcGcM

import Data.List.NonEmpty (NonEmpty(..))

class Memo k where
  data Table k :: * -> *
  tabulate :: (k -> r) -> Table k r
  index :: Table k r -> k -> r

instance Memo () where
  data Table () a = Spot a
    deriving (Functor, Foldable)
  tabulate f = Spot (f ())
  index (Spot x) _ = x

instance Memo Bool where
  data Table Bool a = BoolTable a a
    deriving (Functor, Foldable)
  tabulate f = BoolTable (f True) (f False)
  index (BoolTable x y) = \case
    True -> x
    False -> y

instance (Memo x, Memo y) => Memo (Either x y) where
  data Table (Either x y) a = EitherTable (Table x a) (Table y a)
  tabulate f = EitherTable (tabulate (f . Left)) (tabulate (f . Right))
  index (EitherTable l r) = either (index l) (index r)

instance (Functor (Table x), Functor (Table y), Memo x, Memo y) => Functor (Table (Either x y)) where
  fmap f (EitherTable x y) = EitherTable (fmap f x) (fmap f y)

instance (Foldable (Table x), Foldable (Table y), Memo x, Memo y) => Foldable (Table (Either x y)) where
  foldMap f (EitherTable x y) = foldMap f x `mappend` foldMap f y

instance (Memo x, Memo y) => Memo (x, y) where
  newtype Table (x, y) a = PairTable (Table x (Table y a))
  tabulate f = PairTable (tabulate \x -> tabulate \y -> f (x, y))
  index (PairTable p) (x, y) = index (index p x) y

instance (Foldable (Table x), Foldable (Table y), Memo x, Memo y) => Foldable (Table (x, y)) where
  foldMap f (PairTable t) = foldMap (foldMap f) t

instance (Functor (Table x), Functor (Table y), Memo x, Memo y) => Functor (Table (x, y)) where
  fmap f (PairTable t) = PairTable (fmap (fmap f) t)

instance Memo x => Memo [x] where
  data Table [x] a = ListTable a (Table x (Table [x] a))
  tabulate f = ListTable (f []) (tabulate \x -> tabulate \xs -> f (x : xs))
  index (ListTable a as) = \case
    [] -> a
    (x : xs) -> index (index as x) xs

instance (Foldable (Table x), Memo x) => Foldable (Table [x]) where
  foldMap f (ListTable a as) = f a `mappend` foldMap (foldMap f) as

instance (Functor (Table x), Memo x) => Functor (Table [x]) where
  fmap f (ListTable a as) = ListTable (f a) (fmap (fmap f) as)

instance Memo x => Memo (NonEmpty x) where
  newtype Table (NonEmpty x) a = NonEmptyTable (Table x (Table [x] a))
  tabulate f = NonEmptyTable (tabulate \x -> tabulate \xs -> f (x :| xs))
  index (NonEmptyTable as) (x :| xs) = index (index as x) xs

instance (Foldable (Table x), Memo x) => Foldable (Table (NonEmpty x)) where
  foldMap f (NonEmptyTable as) = foldMap (foldMap f) as

instance (Functor (Table x), Memo x) => Functor (Table (NonEmpty x)) where
  fmap f (NonEmptyTable as) = NonEmptyTable (fmap (fmap f) as)

instance Memo Integer where
  newtype Table Integer a = IntegerTable (Table (NonEmpty Bool) a)
    deriving (Functor, Foldable)
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

memo :: Memo x => (x -> y) -> x -> y
memo = index . tabulate

-- the fibonacci sequence, memoized
fib :: Integer -> Integer
fib = memo fib' where
  fib' 0 = 0
  fib' 1 = 1
  fib' n = fib (n - 1) + fib (n - 2)

-- we are given a grid of size n x m and a function from location in the
-- grid to number of apples we can find there, and we want to know the
-- maximum number of apples we can get by walking down and right towards
-- the 0, 0 coordinates is
maximumApples :: (Integer, Integer) -> ((Integer, Integer) -> Integer) -> Integer
maximumApples (n, m) apples = f (1, 1) where
  f = memo f'
  f' (x, y)
    | x == n && y == m = apples (n, m)
    | x == n = f (x, y + 1) + apples (x, y)
    | y == n = f (x + 1, y) + apples (x, y)
    | otherwise = max (f (x + 1, y)) (f (x, y + 1)) + apples (x, y)

-- the minimum number of deletions and additions required to transform
-- one sequence into another
levenshtein :: Eq a => [a] -> [a] -> Integer
levenshtein a b = f n m where
  n = toInteger $ length a
  m = toInteger $ length b
  f = curry $ memo (uncurry f')
  f' i j | min i j == 0 = max i j
         | otherwise = minimum [ f (i - 1) j + 1
                               , f i (j - 1) + 1
                               , f (i - 1) (j - 1) 
                               + if a !! fromInteger (i - 1) /= b !! fromInteger (j - 1) then 1 else 0 ]

-- the longest common subsequence between two sequences
lcs :: Eq a => [a] -> [a] -> Integer
lcs a b = f n m where
  n = toInteger $ length a
  m = toInteger $ length b
  f = curry $ memo (uncurry f')
  f' i j | i == 0 || j == 0 = 0
         | a !! fromInteger (i - 1) == b !! fromInteger (j - 1) = f (i - 1) (j - 1) + 1
         | otherwise = max (f i (j - 1)) (f (i - 1) j)

data Bound a = Top | Middle a | Bottom
  deriving (Eq, Ord)

instance Memo x => Memo (Bound x) where
  data Table (Bound x) a = BoundTable a a (Table x a)
  tabulate f = BoundTable (f Top) (f Bottom) (tabulate (f . Middle)) 
  index (BoundTable a b t) = \case
    Top -> a
    Bottom -> b
    Middle x -> index t x

floydWarshall :: Integer -> (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
floydWarshall n weight x y = go ((x, y), n) where
  go = memo go'
  go' ((i, j), 0) = if i == j then 0 else weight i j
  go' ((i, j), k) = minimum [ go ((i, j), k - 1), go ((i, k), k - 1) + go ((k, j), k - 1) ]

test :: Bool
test = and [ 
    and [ maximumApples (i, i) (const 1) == i * 2 - 1 | i <- [1..30] ]
  -- ^ if we can find apples everywhere, every path, the maximum in
  -- particular, should have us finding i * 2 - 1 apples.
  , and [ maximumApples (i, i) (\(x, y) -> if x == y then 1 else 0) == i | i <- [1..10] ]
  -- ^ if we can only find apples on the diagonal, we can only find
  -- i apples
  , and [ maximumApples (i, i) (\(x, y) -> if x == i || x == 1 then 1 
                                           else if y == i || y == 1 then 1
                                           else 0) == i * 2 - 1 | i <- [1..30] ]
  -- ^ if we can only find apples along the edges of the grid, we should
  -- only be able to find 2 * i - 1 apples
  , and [ maximumApples (i, i) (\(x, y) -> if x == y then x else 0) == (i * (i + 1)) `div` 2 | i <- [1..10] ]
  -- ^ diagonal path indexed by height will sum to i * (i + 1) / 2 by some
  -- theorem about finite sums from 1 + ... + i
  , and [ fib i == dumbFib 0 1 i | i <- [1..10] ]
  -- ^ A custom fibonacci matches the memoized version
  , and [ levenshtein (mapHead (+ 1) [1..i]) [1..i] == 1
       && levenshtein (take 10 [1..i + 10]) [1..i + 10] == i
       && levenshtein (drop 10 [1..i + 10]) [1..i + 10] == 10
        | i <- [1..5]
        ]
  -- ^ a couple simple property tests on the edit distance
  , and [ lcs [1..i] [i..j] == 1 
       && lcs [1..i] [1..j] == i
       && lcs [1..i] [1..i] == i
       && lcs [1..i] [j..20] == 0
        | i <- [1..10], j <- [11..20] ]
  -- ^ some more property tests on least common subsequence calculation
  , and [ floydWarshall 10 (\i j -> abs (i - j)) a b == abs (a - b) | a <- [1..10], b <- [1..10] ]
  , and [ floydWarshall 10 (\i j -> if abs (i - j) == 1 then 1 else 100000) a b == abs (a - b) | a <- [1..5], b <- [6..10] ]
  -- ^ some property tests about all pairs shortest paths
  ] 
  where
    mapHead :: (x -> x) -> [x] -> [x]
    mapHead f (x : xs) = f x : xs
    mapHead f [] = []
    dumbFib a b 0 = a
    dumbFib a b 1 = b
    dumbFib a b n = dumbFib b (a + b) (n - 1)
