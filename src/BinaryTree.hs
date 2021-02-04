module BinaryTree where

import Data.List (foldl')

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

leaf x = Branch x Empty Empty

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [leaf 'x']
cbalTree n
  | odd n = [Branch 'x' leftSubTree rightSubTree | leftSubTree <- cbalTree (subN `div` 2), rightSubTree <- cbalTree (subN `div` 2)]
  | otherwise =
    [Branch 'x' leftSubTree rightSubTree | leftSubTree <- cbalTree (subN `div` 2), rightSubTree <- cbalTree (subN - subN `div` 2)]
      ++ [Branch 'x' leftSubTree rightSubTree | leftSubTree <- cbalTree (subN - subN `div` 2), rightSubTree <- cbalTree (subN `div` 2)]
  where
    subN = n - 1

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = False
symmetric (Branch _ left right) = mirror left right

construct :: [Int] -> Tree Int
construct = foldl' add Empty
  where
    add Empty x = leaf x
    add (Branch y left right) x =
      case compare x y of
        LT -> Branch y (add left x) right
        _ -> Branch y left (add right x)

reverseTree :: Tree a -> Tree a
reverseTree Empty = Empty
reverseTree (Branch x left right) = Branch x (reverseTree right) (reverseTree left)

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n
  | odd n = [Branch 'x' sub (reverseTree sub) |sub <- cbalTree $ (n - 1) `div` 2]
  | otherwise = []

isLeaf :: Tree a -> Bool 
isLeaf (Branch _ Empty Empty) = True 
isLeaf _ = False

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x : (internals l ++ internals r)

atLevel :: Tree a -> Int -> [a]
atLevel tree level = atLevel' tree level 1
  where
    atLevel' tree level cur 
      | cur == level = 
          case tree of
            (Branch x _ _) -> [x]
            _ -> []
      | cur < level =
          case tree of
            (Branch _ l r) -> atLevel' l level (cur + 1) ++ atLevel' r level (cur + 1)
            _ -> []
      | otherwise  = []

-- atLevel :: Tree a -> Int -> [a]
-- atLevel Empty _ = []
-- atLevel (Branch v l r) n
--     | n == 1 = [v]
--     | n > 1  = atlevel l (n-1) ++ atlevel r (n-1)
--     | otherwise = []