module CampusGraphParser
    ( Node(..)
    , Edge(..)
    , Graph
    , readNodesAndEdges
    ) where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Map.Strict as Map
import Control.Monad (void)
import Text.Parsec.Char (space, newline, char, digit)
import Data.List (foldl')
import Text.Parsec.Combinator (many1, manyTill, eof, skipMany1)
import Text.Parsec.Prim (parse)

-- Type aliases
type Node = (String, Int, Int, Int)  -- (name, nodeId, x, y)
type Edge = (Int, Int, Double)  -- (srcNodeId, dstNodeId, weight)
type Graph = Map.Map Int [(Int, Double)]  -- Map from nodeId to list of (destinationNodeId, weight)

-- Skip spaces and any carriage return or newline
skipWhitespace :: Parser ()
skipWhitespace = skipMany1 (oneOf " \t\r\n")

-- Parse a node (name, id, x, y)
nodeParser :: Parser Node
nodeParser = do
  name <- option "" (manyTill anyChar (char ','))  -- The name can be empty if there is nothing before the comma
  --name <- manyTill anyChar (char ',')  -- Parse node name up to the first comma
  nodeId <- many1 digit  -- Parse node ID
  _ <- char ','  -- Skip comma
  x <- many1 digit  -- Parse x coordinate
  _ <- char ','  -- Skip comma
  y <- many1 digit  -- Parse y coordinate
  many1 (noneOf "\n")
  let finalName = if null name then nodeId else name
  
  return (finalName, read nodeId, read x, read y)

-- Parse a list of nodes (each line represents one node)
nodesParser :: Parser [Node]
nodesParser = endBy1 nodeParser newline

-- Parse an edge (srcNodeId, dstNodeId)
edgeParser :: Parser Edge
edgeParser = do
  srcId <- many1 digit  -- Parse source node ID
  _ <- char ','  -- Skip comma
  dstId <- many1 digit  -- Parse destination node ID
  many1 (noneOf "\n")
  return (read srcId, read dstId, 0)  -- Weight will be calculated later

-- Parse a list of edges (each line represents one edge)
edgesParser :: Parser [Edge]
edgesParser = endBy1 edgeParser newline

-- Calculate the distance between two nodes
calculateDistance :: Node -> Node -> Double 
calculateDistance (_, _, x1, y1) (_, _, x2, y2) =
  sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))

-- Read nodes and edges from files and build the graph
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

  -- Build graph by calculating edge weights based on distance
  let nodeMap = Map.fromList [(nodeId, n) | n@(name, nodeId, x, y) <- nodes]
  let graphWithDistances = foldl' (addEdgeWithDistance nodeMap) (Map.fromList [(nodeId, []) | (_, nodeId, _, _) <- nodes]) edges

  -- Print graph
  putStrLn "\nGraph Structure:"
  mapM_ (\(nodeId, connections) -> do
    putStr $ "Node " ++ show nodeId ++ " connections: "
    putStrLn $ show connections
    ) (Map.toList graphWithDistances)

  return graphWithDistances
  where
    addEdgeWithDistance :: Map.Map Int Node -> Graph -> Edge -> Graph
    addEdgeWithDistance nodeMap graph (src, dst, _) =
      let Just srcNode = Map.lookup src nodeMap
          Just dstNode = Map.lookup dst nodeMap
          dist = calculateDistance srcNode dstNode
      in Map.update (Just . ((dst, dist) :)) src $
         Map.update (Just . ((src, dist) :)) dst graph
