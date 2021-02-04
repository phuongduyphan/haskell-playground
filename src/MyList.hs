module MyList where

import Data.Monoid
import Data.Bifunctor as Bifunctor

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (x : xs) = lastMaybe xs

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe (_ : xs) = Just xs

initMaybe :: [a] -> Maybe [a]
initMaybe xs = fmap reverse $ tailMaybe . reverse $ xs

myUncons :: [a] -> Maybe (a, [a])
myUncons [] = Nothing
myUncons (x : xs) = Just (x, xs)

nullList :: [a] -> Bool
nullList [] = True
nullList _ = False

lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x : xs) = 1 + length xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

myIntersperse :: a -> [a] -> [a]
myIntersperse c [] = []
myIntersperse c (x : xs) = x : c : myIntersperse c xs

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

myIntercalate :: [a] -> [[a]] -> [a]
myIntercalate xs xss = concat $ myIntersperse xs xss

-- [], [1,2,3] [1,2]
myTranspose :: [[a]] -> [[a]]
myTranspose [] = []
myTranspose ([] : xss) = myTranspose xss
myTranspose ((x : xs) : xss) = (x : [h | (h : _) <- xss]) : myTranspose (xs : [t | (_ : t) <- xss])

mySubsequences :: [a] -> [[a]]
mySubsequences [] = [[]]
mySubsequences xs = initSubsequence ++ map (++ [cur]) initSubsequence
  where
    initSubsequence = mySubsequences $ init xs
    cur = last xs

-- "bc", "cb";
myPermutations :: [a] -> [[a]]
myPermutations [] = []
myPermutations [x] = [[x]]
myPermutations (x : xs) = concatMap (interleave [] x) $ myPermutations xs
  where
    interleave :: [a] -> a -> [a] -> [[a]]
    interleave xs x [] = [xs ++ [x]]
    interleave xs x (y : ys) = (xs ++ (x : y : ys)) : interleave (xs ++ [y]) x ys

myFind :: Foldable t => (a -> Bool) -> t a -> Maybe a
myFind p = getFirst . foldMap (\x -> if p x then First $ Just x else First Nothing)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x : xs)
  | pred x = x : myFilter pred xs
  | otherwise = myFilter pred xs

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition _ [] = ([], [])
myPartition pred (x : xs)
  | pred x = Bifunctor.first (x :) partition'
  | otherwise = Bifunctor.second (x : ) partition'
  where
    partition' = myPartition pred xs

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((f, s) : xs) = Bifunctor.bimap (f :) (s :) unzip'
  where unzip' = myUnzip xs
