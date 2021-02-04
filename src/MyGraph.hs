module MyGraph where

import Data.List (foldl', partition)
import Data.Map (Map, fromList, insertWith, toList)

data Graph a = Graph [a] [(a, a)]
  deriving (Show, Eq)

newtype Adj a = Adj [(a, [a])]
  deriving (Show, Eq)

-- graphToAdj :: Ord a => Graph a -> Adj a
-- graphToAdj (Graph v edges) = Adj $ toList $ foldl' (\curMap (k, x) -> insertWith (++) k [x] curMap) adjMap edges
--   where
--     adjMap = fromList $ map (, []) v

graphToAdj :: Eq a => Graph a -> Adj a
graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (x : xs) ys) = Adj $ (x, ys >>= f) : zs
  where
    f (a, b)
      | a == x = [b]
      | b == x = [a]
      | otherwise = []
    Adj zs = graphToAdj (Graph xs ys)

paths :: Int -> Int -> [(Int, Int)] -> [[Int]]
paths a b g = paths' a b g []
  where
    paths' a b g visited
      | a == b = [[a]]
      | otherwise = foldl' (\acc (_, e) -> acc ++ map (a :) (paths' e b g (a : visited))) [] $ filter (\(x, _) -> x == a && x `notElem` visited) g


-- path [1, 4, 2, 3, 2, 3]
-- pop until x == a

-- partition [(a, 1), (a, 3), (a, 5), rest]
cycleG :: Int -> [(Int, Int)] -> [[Int]]
cycleG s g = cycle' s [] g
  where
    cycle' :: Int -> [Int] -> [(Int, Int)] -> [[Int]]
    cycle' s visited g -- 2 [2, 3, 4]
      | s `elem` visited = [pop s visited ++ [s]]
      | otherwise = do
        (_, next) <- filter ((==) s . fst) g -- (4,2) (4,3)
        cycle' next (visited ++ [s]) g
    
pop s [] = []
pop s (x : xs)
  | s == x = x : xs
  | otherwise = pop s xs