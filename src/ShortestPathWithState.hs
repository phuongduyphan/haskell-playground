module ShortestPathWithState where

import Control.Monad (unless)
import Control.Monad.State (State, get, put, state, execState, guard)
import Data.Heap (MinHeap)
import qualified Data.Heap as Heap
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Dist a = Map a Int

newtype Graph a = Graph (Map a [(a, Int)])
  deriving (Show)

initPq :: Ord a => a -> MinHeap (Int, a)
initPq s = Heap.singleton (0, s)

runDijkstra :: Ord a => Graph a -> a -> a -> Int
runDijkstra (Graph g) s d = fromJust $ Map.lookup d resultDist
  where
    (resultDist, _) = execState (dijkstra (Graph g)) (dist, pq)
    dist = let d = Map.fromList $ map (\(u, _) -> (u, maxBound :: Int)) graphList in Map.insert s 0 d
    pq = initPq s
    graphList = Map.toList g

dijkstra :: Ord a => Graph a -> State (Dist a, MinHeap (Int, a)) ()
dijkstra (Graph g) = do
  (dist, pq) <- get
  unless (Heap.null pq) $ do
    let ((d, u), pq') = fromJust $ Heap.view pq
    put (dist, pq')
    if d > fromJust (Map.lookup u dist)
      then do
        dijkstra (Graph g)
      else do
        update (Graph g) u 0
        dijkstra (Graph g)

update :: Ord a => Graph a -> a -> Int -> State (Dist a, MinHeap (Int, a)) ()
update (Graph g) u j
  | j < length (fromJust (Map.lookup u g)) = do
    (dist, pq) <- get
    if fromJust (Map.lookup u dist) + vDist < fromJust (Map.lookup v dist)
      then
        let dist' = Map.insert v (dist ! u + vDist) dist
            pq' = Heap.insert (dist' ! v, v) pq
         in do
              put (dist', pq')
              update (Graph g) u (j + 1)
      else update (Graph g) u (j + 1)
  | otherwise = return ()
  where
    (v, vDist) = fromJust (Map.lookup u g) !! j

graph = Graph $ Map.fromList [(1, [(3, 3), (4, 6)]), (0, [(4, 1)]), (2, [(3, 7), (0, 6), (1, 2)]), (3, [(4, 5)]), (4, [])]