module Dijkstra (dijkstra, Graph, Node) where

import qualified Data.Map.Strict as Map
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, fromJust)

type Node = String
type Graph = Map.Map Node [(Node, Int)]

dijkstra :: Graph -> Node -> Map.Map Node (Maybe Int)
dijkstra graph start =
  let 
    -- Initialize distances: start node = Just 0, others = Nothing
    initSP = Map.fromList [(n, if n == start then Just 0 else Nothing) | (n, _) <- Map.toList graph]
    
    -- Find the next unvisited node with the smallest distance
    nextNode sp unvisited = 
      let validNodes = [(n, d) | n <- unvisited, 
                                let d = Map.findWithDefault Nothing n sp,
                                isJust d]
      in case validNodes of
           [] -> Nothing
           xs -> Just $ fst $ minimumBy (comparing (fromJust . snd)) xs
    
    -- Process a single node and its neighbors
    processNode sp current unvisited =
      let 
        neighbors = Map.findWithDefault [] current graph
        currentDist = fromJust $ Map.findWithDefault Nothing current sp
        
        -- Update distances to neighbors
        newSP = foldl' (\acc (neighbor, weight) ->
                  let newDist = currentDist + weight
                      oldDist = Map.findWithDefault Nothing neighbor acc
                  in case oldDist of
                       Nothing -> Map.insert neighbor (Just newDist) acc
                       Just old -> Map.insert neighbor (Just $ min old newDist) acc
                ) sp neighbors
      in (newSP, filter (/= current) unvisited)
    
    -- Main algorithm loop
    go sp [] = sp
    go sp unvisited = 
      case nextNode sp unvisited of
        Nothing -> sp  -- No more reachable nodes
        Just current -> 
          let (newSP, newUnvisited) = processNode sp current unvisited
          in go newSP newUnvisited
    
    -- Initial unvisited nodes list
    initialUnvisited = Map.keys graph
  in go initSP initialUnvisited