{-# LANGUAGE BlockArguments #-}
module Main where

import Examples
import Data.Char (toLower)
import Control.Concurrent
import Control.Monad

race :: IO a -> IO b -> IO (Either a b)
race a b = do
  result <- newEmptyMVar
  at <- forkIO ((Left <$> a) >>= putMVar result)
  bt <- forkIO ((Right <$> b) >>= putMVar result)
  r <- takeMVar result
  case r of
    Left _ -> r <$ killThread bt
    Right _ -> r <$ killThread at

describe :: String -> Bool -> IO ()
describe description b = do
  putStr ("  " <> description <> "...")
  void $ race (forever (threadDelay 10000 >> putStr ".")) $ putStrLn (if not b then "\x1b[31mFAILED\x1b[0m" else "\x1b[32mSUCCEEDED\x1b[0m")

section :: String -> IO () -> IO ()
section name s = do
  putStrLn ("\x1b[33m" <> name <> "\x1b[0m:")
  s

main :: IO ()
main = do
  appleTests
  fibTests
  levenshteinTests
  longestCommonSubsequenceTests
  longestCommonSubstringTests
  floydWarshallTests
  subsetSumTests
  coinChangeTests

appleTests :: IO ()
appleTests = section "Apple" do
  describe "apples everywhere"
    $ and [ maximumApples (i, i) (const 1) == i * 2 - 1 | i <- [1..30] ]
  describe "apples on diagonal"
    $ and [ maximumApples (i, i) (\(x, y) -> if x == y then 1 else 0) == i | i <- [1..10] ] 
  describe "apples on edges"
    $ and [ maximumApples (i, i) (\(x, y) -> if x == i || x == 1 then 1 
                                           else if y == i || y == 1 then 1
                                           else 0) == i * 2 - 1 | i <- [1..30] ]
  describe "sum from 1 to i = i * (i + 1) / 2"
    $ and [ maximumApples (i, i) (\(x, y) -> if x == y then x else 0) == (i * (i + 1)) `div` 2 | i <- [1..10] ]

fibTests :: IO ()
fibTests = section "Fib" do
  describe "fib matches naive fib"
    $ and [ fib i == dumbFib 0 1 i | i <- [1..10] ] 
  where
    dumbFib a b 0 = a
    dumbFib a b 1 = b
    dumbFib a b n = dumbFib b (a + b) (n - 1)
      

levenshteinTests :: IO ()
levenshteinTests = section "Levenshtein" do
  describe "one edit has distance 1"
    $ and [ levenshtein (mapHead (+ 1) [1..i]) [1..i] == 1 | i <- [1..5] ]
  describe "removing increases edit distance by 1"
    $ and [ levenshtein (take 10 [1..i + 10]) [1..i + 10] == i 
         && levenshtein (drop 10 [1..i + 10]) [1..i + 10] == 10 
         | i <- [1..5] ]
  where
    mapHead :: (x -> x) -> [x] -> [x]
    mapHead f (x : xs) = f x : xs
    mapHead f [] = []

longestCommonSubsequenceTests :: IO ()
longestCommonSubsequenceTests = section "Longest Common Subsequence" do
  describe "intersection with 1 element"
    $ and [ longestCommonSubsequence [1..i] [i..j] == 1 | i <- [1..10], j <- [11..20] ]
  describe "intersection with i elements"
    $ and [ longestCommonSubsequence [1..i] [1..j] == i | i <- [1..10], j <- [11..20] ]
  describe "same sequence"
    $ and [ longestCommonSubsequence [1..i] [1..i] == i | i <- [1..10] ]
  describe "disjoint sequences"
    $ and [ longestCommonSubsequence [1..i] [j..20] == 0 | i <- [1..10], j <- [11..20] ]
  
floydWarshallTests :: IO ()
floydWarshallTests = section "Floyd Warshall" do
  describe "euclidean"
    $ and [ floydWarshall 5 (\i j -> abs (i - j)) a b == abs (a - b) | a <- [1..5], b <- [1..5] ]
  describe "discrete-ish"
    $ and [ floydWarshall 10 (\i j -> if abs (i - j) == 1 then 1 else 100000) a b == abs (a - b) | a <- [1..5], b <- [6..10] ]

longestCommonSubstringTests :: IO ()
longestCommonSubstringTests = section "Longest Common Substring" do
  describe "intersection with 1 element"
    $ and [ longestCommonSubstring [1..i] [i..j] == 1 | i <- [1..10], j <- [11..20] ]
  describe "same elements, but different lengths"
    $ and [ longestCommonSubstring (take i (repeat 'x')) (take j (repeat 'x')) == toInteger (max i j) - abs (toInteger i - toInteger j) | i <- [1..10], j <- [1..10] ]

subsetSumTests :: IO ()
subsetSumTests = section "Subset Sum" do
  describe "target in list"
    $ and [ subsetSum [1..i] j | i <- [1..10], j <- [1..i] ]
  describe "binary representation"
    $ and [ subsetSum ((2^) <$> [0..i]) j | i <- [1..4], j <- [1..2^i] ]
  describe "cant represent 2^(i + 1) in binary with i bits"
    $ and [ not $ subsetSum ((2^) <$> [0..i]) (2^(i + 1)) | i <- [1..4] ]
  describe "can represent 2^(i + 1) - 1 in binary with i bits"
    $ and [ subsetSum ((2^) <$> [0..i]) (2^(i + 1) - 1) | i <- [1..4] ] 

coinChangeTests :: IO ()
coinChangeTests = section "Coin Change" do
  describe "only one way to make zero"
    $ and [ coinChange 0 [1..i] == 1 | i <- [1..10] ]
  describe "no way to make a negative number"
    $ and [ coinChange (-i) [1..i] == 0 | i <- [1..10] ]
  describe "some known instances"
    $ and [ coinChange 4 [1, 2, 3] == 4
          , coinChange 5 [1, 2, 3] == 5 ]
