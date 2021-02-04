module ShortestPath where

import Data.Heap (MinHeap)
import qualified Data.Heap as Heap
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

newtype Graph a = Graph (Map a [(a, Int)])
  deriving (Show)

initPq :: Ord a => a -> MinHeap (Int, a)
initPq s = Heap.singleton (0, s)

dijkstra :: Ord a => Graph a -> a -> a -> Int
dijkstra (Graph g) s d = fromJust $ Map.lookup d (dijkstra' (Graph g) dist pq)
  where
    dist = let d = Map.fromList $ map (\(u, _) -> (u, maxBound :: Int)) graphList in Map.insert s 0 d
    pq = initPq s
    graphList = Map.toList g

dijkstra' :: Ord a => Graph a -> Map a Int -> MinHeap (Int, a) -> Map a Int
dijkstra' (Graph g) dist pq 
  | Heap.isEmpty pq = dist
  | otherwise =
      if d > fromJust (Map.lookup u dist) then 
        dijkstra' (Graph g) dist pq'
      else
        let (dist', pq'') = update (Graph g) u 0 dist pq'
        in dijkstra' (Graph g) dist' pq''
      where 
        ((d, u), pq') = fromJust $ Heap.view pq

update :: Ord a => Graph a -> a -> Int -> Map a Int -> MinHeap (Int, a) -> (Map a Int, MinHeap (Int, a))
update (Graph g) u j dist pq
  | j >= length (fromJust (Map.lookup u g)) = (dist, pq)
  | otherwise = 
    if fromJust (Map.lookup u dist) + vDist < fromJust (Map.lookup v dist) then
      let dist' = Map.insert v (dist!u + vDist) dist
          pq' = Heap.insert (dist'!v, v) pq
      in update (Graph g) u (j + 1) dist' pq'
    else 
      update (Graph g) u (j + 1) dist pq 
  where
    (v, vDist) = fromJust (Map.lookup u g) !! j

graph = Graph $ Map.fromList [(1, [(3, 3), (4, 6)]), (0, [(4, 1)]), (2, [(3, 7), (0, 6), (1, 2)]), (3, [(4, 5)]), (4, [])]