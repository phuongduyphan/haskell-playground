module NinetyNineProbsList where

import Control.Monad (replicateM)
import Data.List (group, sortBy, sortOn)
import Data.Map(fromListWith, (!))
import System.Random
import MyList(myPermutations)

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs idx = h ++ [x] ++ t
  where
    (h, t) = splitAt (idx - 1) xs

myLast :: [a] -> a
myLast = foldr1 $ const id

myButLast :: [a] -> a
myButLast [] = error "Unexpected empty list"
myButLast [x] = error "Unexpected one element list"
myButLast [x, xs] = x
myButLast (x : xs) = myButLast xs

-- myButLast :: [a] -> a
-- myButLast = last . init

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- compress :: Eq a => [a] -> [a]
-- compress = foldl appendDup []
--   where
--     appendDup :: Eq a => [a] -> a -> [a]
--     appendDup [] x = [x]
--     appendDup xs x
--       | last xs == x = xs
--       | otherwise = xs ++ [x]

compress :: Eq a => [a] -> [a]
compress = map head . group

pack :: Eq a => [a] -> [[a]]
pack = group

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\sub -> (length sub, head sub)) $ pack xs

-- [(length (x:xs), x) | (x:xs) <- xs]

data ElementState a = Multiple Int a | Single a
  deriving (Show)

encodeModified :: Eq a => [a] -> [ElementState a]
encodeModified = map encodeHelper . encode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (len, x) = Multiple len x

decodeModified :: [ElementState a] -> [a]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple len x) = replicate len x

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = drop' 1 xs n
  where
    drop' p (x : xs) n
      | p `mod` n == 0 = drop' (p + 1) xs n
      | otherwise = x : drop' (p + 1) xs n
    drop' _ [] _ = []

mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n = splitAt n xs

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j - i + 1) $ drop (i - 1) xs

rotate :: [a] -> Int -> [a]
rotate xs n
  | n >= 0 = drop n xs ++ take n xs
  | otherwise = drop (length xs + n) xs ++ take (length xs + n) xs

removeAt :: Int -> [a] -> (a, [a])
removeAt x xs
  | 1 <= x && x <= length xs = (last h, init h ++ t)
  | otherwise = error "x is out of bound"
  where
    (h, t) = splitAt x xs

range :: Int -> Int -> [Int]
range i j = [i .. j]

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
  | n < 0 = error "n must >= 0"
  | otherwise = do
    pos <- replicateM n $ getStdRandom $ randomR (0, length xs - 1)
    return $ map (xs !!) pos

diffSelect :: Int -> Int -> IO [Int]
diffSelect n bound
  | n < 0 = error "n must >= 0"
  | otherwise = do
    replicateM n $ getStdRandom $ randomR (1, bound)

rndPermu :: [a] -> IO [a]
rndPermu xs =
  fmap (permuts !!) $ getStdRandom $ randomR (0, length permuts - 1)
  where
    permuts = myPermutations xs

permutationsWithN :: Int -> [a] -> [[a]]
permutationsWithN _ [] = [[]]
permutationsWithN 1 xs = map (: []) xs
permutationsWithN k list@(x : xs) 
  | k <= length list = concatMap (interleave x []) (permutationsWithN (k - 1) xs) ++ permutationsWithN k xs
  | otherwise = []
  where
    interleave x xs [] = [xs ++ [x]]
    interleave x xs rem@(y : ys) = (xs ++ [x] ++ rem) : interleave x (xs ++ [y]) ys

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = map (: []) xs
combinations k list@(x : xs) 
  | k <= length list = map (x :) (combinations (k - 1) xs) ++ combinations k xs
  | otherwise = []

lsort :: [[a]] -> [[a]]
lsort = sortOn length

-- "abc", "de", "fgh"
-- 3, 2, 3, 1, 2, 3

lfsort :: [[a]] -> [[a]]
lfsort xs = sortOn ((!) lengthFreqMap . length) xs
  where
    lengthFreqMap = fromListWith (+) $ map (\x -> (length x, 1)) xs
    