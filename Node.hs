module Node (Node(..), Edge(..), Graph(..), buildGraph, addEdge) where

data Node = Node { nodeName :: String, edges :: [Edge] }
  deriving (Eq, Show)

data Edge = Edge { targetNode :: Node, weight :: Int }
  deriving (Eq, Show)

data Graph = Graph [Node] deriving (Eq, Show)

buildGraph :: [Node] -> Graph
buildGraph nodes = Graph nodes

addEdge :: Node -> Node -> Int -> Node
addEdge source target weight = 
  let edge = Edge target weight
  in source { edges = edge : edges source }
