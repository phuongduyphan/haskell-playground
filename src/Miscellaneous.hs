module Miscellaneous where

-- [1, 0, 0, 1]
-- [1, 1, 0, 0]
-- [0, 1, 1, 0]
-- [0, 0, 0, 0]

reach :: (Int, Int) -> [(Int, Int)] -> [[Int]] -> [(Int, Int)]
reach src visited graph = filter (\(r, c) -> isInBound (r,c ) && graph!!r!!c == 1 && notElem (r, c) visited)  $ move src
  where 
    move (r, c) = [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
    isInBound (r, c) = r >= 0 && r < length graph && c >= 0 && c < length (head graph)

canGo :: [[Int]] -> (Int, Int) -> (Int, Int) -> Bool
canGo graph src dest = canGo' graph src dest []
  where
    canGo' :: [[Int]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Bool
    canGo' graph src dest visited = elem dest next || any (\nxt -> canGo' graph nxt dest (visited ++ next)) next
      where next = reach src visited graph