module GraphParser 
    ( Node
    , Edge
    , Graph
    , readNodesAndEdges
    ) where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Map.Strict as Map
import Control.Monad (void)

-- Type aliases
type Node = String
type Edge = (Node, Node, Int)
type Graph = Map.Map Node [(Node, Int)]

-- Parsec parser for node file
nodesParser :: Parser [Node]
nodesParser = endBy1 nodeParser newline
  where
    nodeParser = many1 (noneOf "\n")

-- Parsec parser for edge file
edgesParser :: Parser [Edge]
edgesParser = endBy1 edgeParser newline
  where
    edgeParser = do
      src <- many1 (noneOf " \t")
      skipMany1 space
      dst <- many1 (noneOf " \t")
      skipMany1 space
      weight <- many1 digit
      return (src, dst, read weight)

-- Read nodes and edges using Parsec
readNodesAndEdges :: FilePath -> FilePath -> IO Graph
readNodesAndEdges nodeFile edgeFile = do
  -- Parse nodes
  nodesResult <- parseFromFile nodesParser nodeFile
  nodes <- case nodesResult of
    Left err -> error $ "Error parsing nodes: " ++ show err
    Right parsedNodes -> return parsedNodes
  
  -- Parse edges
  edgesResult <- parseFromFile edgesParser edgeFile
  edges <- case edgesResult of
    Left err -> error $ "Error parsing edges: " ++ show err
    Right parsedEdges -> return parsedEdges
  
  -- Convert to graph
  return $ foldl addEdge (Map.fromList [(n, []) | n <- nodes]) edges
  where
    addEdge :: Graph -> Edge -> Graph
    addEdge g (s, d, w) = Map.update (Just . ((d, w) :)) s g
