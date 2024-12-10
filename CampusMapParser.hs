module CampusMapParser
    ( Node(..)
    , Edge(..)
    , Graph
    , campusReadNodesAndEdges
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
type Node = (String, String, Int, Int)  -- (name, nodeId, x, y)
type Edge = (String, String, Int)  -- (srcNodeId, dstNodeId, weight)
type Graph = Map.Map String [(String, Int)]  -- Map from node name to list of (destination node name, weight)

-- Parse a node (name, id, x, y)
nodeParser :: Parser Node
nodeParser = do
  name <- option "" (manyTill anyChar (char ','))  -- The name can be empty if there is nothing before the comma
  nodeId <- many1 (alphaNum <|> char '_')  -- Parse node ID (allowing alphanumeric and underscore)
  _ <- char ','  -- Skip comma
  x <- many1 digit  -- Parse x coordinate
  _ <- char ','  -- Skip comma
  y <- many1 digit  -- Parse y coordinate
  many1 (noneOf "\n")
  let finalName = if null name then nodeId else name
  
  return (finalName, nodeId, read x, read y)

-- Parse a list of nodes (each line represents one node)
nodesParser :: Parser [Node]
nodesParser = endBy1 nodeParser newline

-- Parse an edge (srcNodeId, dstNodeId)
edgeParser :: Parser Edge
edgeParser = do
  srcId <- many1 (alphaNum <|> char '_')  -- Parse source node ID
  _ <- char ','  -- Skip comma
  dstId <- many1 (alphaNum <|> char '_')  -- Parse destination node ID
  many1 (noneOf "\n")
  return (srcId, dstId, 0)  -- Weight will be calculated later

-- Parse a list of edges (each line represents one edge)
edgesParser :: Parser [Edge]
edgesParser = endBy1 edgeParser newline

-- Calculate the distance between two nodes
calculateDistance :: Node -> Node -> Int
calculateDistance (_, _, x1, y1) (_, _, x2, y2) =
  round $ sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))

-- Read nodes and edges from files and build the graph
campusReadNodesAndEdges :: FilePath -> FilePath -> IO Graph
campusReadNodesAndEdges nodeFile edgeFile = do
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
  let nodeNameMap = Map.fromList [(nodeId, name) | (name, nodeId, _, _) <- nodes]
  
  let graphWithDistances = foldl' (addEdgeWithDistance nodeMap nodeNameMap) 
                           (Map.fromList [(name, []) | (name, _, _, _) <- nodes]) 
                           edges
  
  
  return graphWithDistances
  where
    addEdgeWithDistance :: Map.Map String Node -> Map.Map String String -> Graph -> Edge -> Graph
    addEdgeWithDistance nodeMap nodeNameMap graph (src, dst, _) =
      let Just srcNode = Map.lookup src nodeMap
          Just dstNode = Map.lookup dst nodeMap
          Just srcName = Map.lookup src nodeNameMap
          Just dstName = Map.lookup dst nodeNameMap
          dist = calculateDistance srcNode dstNode
      in Map.update (Just . ((dstName, dist) :)) srcName $
         Map.update (Just . ((srcName, dist) :)) dstName graph
