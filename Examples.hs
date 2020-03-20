{-# LANGUAGE TypeOperators #-}
module Examples 
  ( fib
  , maximumApples
  , levenshtein
  , longestCommonSubsequence
  , floydWarshall
  , longestCommonSubstring
  , subsetSum 
  , coinChange ) where

import GHC.Generics
import Data.Functor.Const
import Memo
import Data.List (sort)

-- we compute the fibonacci sequence
fib :: Integer -> Integer
fib = memo fib' where
  fib' 0 = 0
  fib' 1 = 1
  fib' n | n < 0 = error "fibonacci is not defined on negative numbers"
         | otherwise =  fib (n - 1) + fib (n - 2)

-- we are given a grid of size n x m and a function from location in the
-- grid to number of apples we can find there, and we want to know the
-- maximum number of apples we can get by walking down and right towards
-- the (1, 1) coordinates is maximumApples (n, m).
maximumApples :: (Integer, Integer) -> ((Integer, Integer) -> Integer) -> Integer
maximumApples (n, m) apples = f (1, 1) where
  f = memo f'
  -- we define a function which is the maximum amount of apples we could
  -- have picked up at the given coordinates
  f' (x, y)
    | x == n && y == m = apples (n, m)
    -- ^ if we are in the bottom right corner, we pick up our final apples
    | x == n = f (x, y + 1) + apples (x, y)
    -- ^ if we are on the right wall, we can only move down and pick up our
    -- apples
    | y == n = f (x + 1, y) + apples (x, y)
    -- ^ if we are on the bottom wall, we can only move right and pick up
    -- our apples
    | otherwise = max (f (x + 1, y)) (f (x, y + 1)) + apples (x, y)
    -- ^ otherwise, we take the maximum of having come from above or the
    -- left, along with whatever apples we can pick up where we are

-- the minimum number of edits required to transform one sequence into another
levenshtein :: Eq a => [a] -> [a] -> Integer
levenshtein a b = f n m where
  n = toInteger $ length a
  m = toInteger $ length b
  f = curry $ memo (uncurry f')
  f' i j | min i j == 0 = max i j
         -- ^ if we have reached the end of one string, we must insert the
         -- rest of the characters of the other to reconstruct it
         | otherwise = minimum [ f (i - 1) j + 1
                               , f i (j - 1) + 1
                               , f (i - 1) (j - 1) 
                               + if a !! fromInteger (i - 1) /= b !! fromInteger (j - 1) then 1 else 0 ]
        -- ^ otherwise, we must take the minimum cost of deleting a character in
        -- the one or the other string, changing the character on the
        -- current index and moving on. If the characters are the same, the
        -- cost is 0, otherwise it is 1. this code is straightforwardly
        -- modified to allow for differing costs for deleting and modifying
        -- characters

-- the length of the longest common subsequence between two sequences
longestCommonSubsequence :: Eq a => [a] -> [a] -> Integer
longestCommonSubsequence a b = f n m where
  n = toInteger $ length a
  m = toInteger $ length b
  f = curry $ memo (uncurry f')
  f' i j | i == 0 || j == 0 = 0
         | a !! fromInteger (i - 1) == b !! fromInteger (j - 1) = f (i - 1) (j - 1) + 1
         | otherwise = max (f i (j - 1)) (f (i - 1) j)

-- floydWarshall n d x y is equal to the shortest path between x and y in
-- the n node fully connected graph with distance function d
floydWarshall :: Integer -> (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
floydWarshall n weight x y = go ((x, y), n) where
  go = memo go'
  go' ((i, j), 0) = if i == j then 0 else weight i j
  go' ((i, j), k) = minimum [ go ((i, j), k - 1), go ((i, k), k - 1) + go ((k, j), k - 1) ]

-- subsetSum xs target = True if and only if there is a subsequence of xs
-- that sums to target
subsetSum :: [Integer] -> Integer -> Bool
subsetSum xs target = go n target where
  n = toInteger $ length xs
  go = curry $ memo (uncurry go')
  -- we define a function which tells us whether or not we can make the
  -- given target out of the subset of the values in our sequence before index i
  go' i target | target == 0 = True
               -- ^ if the target is 0, we can always build a subset sum to equal
               -- it by using the empty set
               | i < 1 = False
               -- ^ if the target is nonzero and we have no elements left,
               -- we cannot build it
               | otherwise = go (i - 1) target || go (i - 1) (target - (xs !! (fromInteger i - 1)))
               -- ^ otherwise, the ability to construct our target means
               -- that either we can build it with all the elements
               -- but the last, or we can build target - the last
               -- element using the rest of the elements

-- the longest common substring between two lists
longestCommonSubstring :: Eq a => [a] -> [a] -> Integer
longestCommonSubstring a b = maximum [ f i j | i <- [1..n], j <- [1..m] ] where
  n = toInteger $ length a
  m = toInteger $ length b
  f = curry $ memo (uncurry f')
  f' i j | i == 0 || j == 0 = 0
         -- ^ if 
         | a !! fromInteger (i - 1) == b !! fromInteger (j - 1) = f (i - 1) (j - 1) + 1
         | otherwise = 0

-- given a value n, if we want to make change for N and we have an infinite
-- supply of coins valued s1, s2, ..., sm, how many ways can we make the
-- change?
coinChange :: Integer -> [Integer] -> Integer
coinChange n s = f n m where
  m = toInteger $ length s
  f = curry (memo (uncurry f'))
  -- we define a function which tells us the number of ways to make change
  -- for a specific number given a certain maximum coin index
  f' n' i | n' == 0 = 1
          -- ^ there is one way to construct no value, by including no
          -- coins
          | n' < 0 || (i <= 0 && n' >= 1) = 0
          -- ^ we can never construct a negative value, and we cannot
          -- construct a positive value with no coins
          | otherwise = f n' (i - 1) + f (n' - si) i where
              si = s !! fromInteger (i - 1)
          -- ^ otherwise, we note that we can separate our cases into those
          -- where we use the ith coin next and those where we don't, and
          -- as these are disjoint and fully capture the ways forward, we
          -- can sum them together to get the total number of ways to make
          -- the change

maximumPathSum :: Fix (Const Integer :*: []) -> Integer
maximumPathSum = go where
  go = memo go'
  go' (Fix (Const n :*: [])) = n
  go' (Fix (Const n :*: leaves)) = maximum (((+ n) . go) <$> leaves)
