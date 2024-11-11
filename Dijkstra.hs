module Dijkstra (dijkstra) where

import Node (Node(..), Graph(..), Edge(..))

dijkstra :: Graph -> Node -> [(Node, Int)]
dijkstra (Graph nodes) start = dijkstraHelper nodes [(start, 0)] []

dijkstraHelper :: [Node] -> [(Node, Int)] -> [Node] -> [(Node, Int)]
dijkstraHelper [] _ _ = []  
dijkstraHelper unvisited distances visited =
  let (currentNode, currentDist) = findMinNode distances  
      neighbors = edges currentNode
      updatedDistances = foldl (updateDistances currentNode currentDist) distances neighbors
      unvisited' = filter ((/= nodeName currentNode) . nodeName . fst) unvisited
  in if null unvisited' then distances else dijkstraHelper unvisited' updatedDistances (currentNode : visited)

findMinNode :: [(Node, Int)] -> (Node, Int)
findMinNode = minimumBy (\(_, dist1) (_, dist2) -> compare dist1 dist2)

updateDistances :: Node -> Int -> [(Node, Int)] -> Edge -> [(Node, Int)]
updateDistances currentNode currentDist distances (Edge targetNode weight) =
  let newDist = currentDist + weight
  in if newDist < (getDistance targetNode distances) 
     then (targetNode, newDist) : distances
     else distances

getDistance :: Node -> [(Node, Int)] -> Int
getDistance node distances = 
  case lookup node distances of
    Just dist -> dist
