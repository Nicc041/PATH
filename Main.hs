module Main where

import Node (Node(..), Edge(..), Graph(..), buildGraph, addEdge)
import Dijkstra (dijkstra)

-- Sample data for nodes and edges
createSampleGraph :: Graph
createSampleGraph = 
  let a = Node "A" []
      b = Node "B" []
      c = Node "C" []
      d = Node "D" []
      e = Node "E" []
  in buildGraph [addEdge a b 4, addEdge a c 2, addEdge b c 5, addEdge b d 10, 
                 addEdge c d 3, addEdge c e 6, addEdge d e 1]

-- Main function
main :: IO ()
main = do
  let graph = createSampleGraph
      startNode = Node "A" []
      result = dijkstra graph startNode
  putStrLn "Shortest distances from the start node:"
  mapM_ printResult result

-- Print the result
printResult :: (Node, Int) -> IO ()
printResult (node, dist) = putStrLn $ nodeName node ++ ": " ++ show dist
