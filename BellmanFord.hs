module BellmanFord (bellmanFord, Graph, Node, Edge) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

type Node = String
type Edge = (Node, Node, Int)
type Graph = Map.Map Node [(Node, Int)]

-- Converts a Graph (adjacency list) into a list of edges
graphToEdges :: Graph -> [Edge]
graphToEdges graph = 
  [(src, dst, weight) | (src, neighbors) <- Map.toList graph, (dst, weight) <- neighbors]

bellmanFord :: Graph -> Node -> Either String (Map.Map Node (Maybe Int))
bellmanFord graph start =
  let
    -- Initialize distances: start node = 0, others = Nothing
    initialDistances = Map.fromList [(n, if n == start then Just 0 else Nothing) | n <- Map.keys graph]
    
    -- Relax all edges |V| - 1 times
    relax edges distances =
      foldl update distances edges
      where
        update dist (src, dst, weight) =
          let srcDist = Map.findWithDefault Nothing src dist
              dstDist = Map.findWithDefault Nothing dst dist
          in case srcDist of
               Just s -> 
                 let newDist = s + weight
                 in if dstDist == Nothing || newDist < fromMaybe maxBound dstDist
                    then Map.insert dst (Just newDist) dist
                    else dist
               Nothing -> dist

    -- Check for negative weight cycles
    hasNegativeCycle edges distances =
      any (\(src, dst, weight) -> 
        let srcDist = Map.findWithDefault Nothing src distances
            dstDist = Map.findWithDefault Nothing dst distances
        in case srcDist of
             Just s -> fromMaybe maxBound dstDist > s + weight
             Nothing -> False
          ) edges

    edges = graphToEdges graph
    relaxedDistances = iterate (relax edges) initialDistances !! (length (Map.keys graph) - 1)
  in
    if hasNegativeCycle edges relaxedDistances
    then Left "Graph contains a negative weight cycle"
    else Right relaxedDistances
