module Arithmetic where

import Data.List(group, foldl', find)
import Data.Maybe(fromJust)

isPrime :: Int -> Bool
isPrime n = all (\x -> n `mod` x /= 0) [2..floor . sqrt $ fromIntegral n]

myGcd :: Int -> Int -> Int
myGcd a b
  | b == 0 = a
  | a > b = gcd b (a `mod` b)
  | otherwise = gcd a (b `mod` a)

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

totient :: Int -> Int
totient n = length $ filter (coprime n) [1..n - 1]

primeFactors :: Int -> [Int]
primeFactors n = pf n 2
  where
    pf 1 _ = []
    pf n x
      | n `mod` x == 0 = x : pf (n `div` x) x
      | otherwise  = pf n (x + 1)

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map (\g -> (head g, length g)) $ group (primeFactors n)

totientImprove :: Int -> Int 
totientImprove x = foldl' (\acc (p, m) -> acc * (p - 1) * p ^ (m - 1)) 1 $ primeFactorsMult x

primesR :: Int -> Int -> [Int]
primesR lo hi = filter isPrime [lo..hi]

goldbach :: Int -> (Int, Int)
goldbach n
  | n > 2 && even n = (fromJust maybeA, n - fromJust maybeA)
  | otherwise = error "Invalid n"
  where maybeA = find (\x -> isPrime x && isPrime (n - x)) [2..n]

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList lo hi = map goldbach $ filter even [lo..hi]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' lo hi x = filter (\(a, b) -> a > x && b > x) $ map goldbach $ filter even [lo..hi]