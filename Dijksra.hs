import Data.Char (isSpace)
import Data.List (foldl', minimumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

-- Represent a node as a String
type Node = String

-- Represent an edge as a triple of source, destination, and weight
type Edge = (Node, Node, Int)

-- Represent the graph as a map of nodes to their adjacent nodes and weights
type Graph = Map.Map Node [(Node, Int)]

-- Parse a line of nodes file
parseNode :: String -> Node
parseNode = dropWhile isSpace

-- Parse a line of edges file
parseEdge :: String -> Edge
parseEdge line =
  let [src, dst, w] = words line
  in (src, dst, read w)

-- Read nodes and edges from files
readNodesAndEdges :: FilePath -> FilePath -> IO Graph
readNodesAndEdges nodeFile edgeFile = do
  nodes <- lines <$> readFile nodeFile
  edges <- map parseEdge . lines <$> readFile edgeFile
  return $ foldl' addEdge (Map.fromList [(n, []) | n <- nodes]) edges
  where
    addEdge :: Graph -> Edge -> Graph
    addEdge g (s, d, w) = Map.update (Just . ((d, w) :)) s g

-- Dijkstra's algorithm
dijkstra :: Graph -> Node -> Map.Map Node Int
dijkstra graph start =
  let
    initSP = Map.fromList [(n, if n == start then 0 else maxBound) | (n, _) <- Map.toList graph]
    nextNode sp = minimumBy (comparing snd) [(n, d) | (n, d) <- Map.toList sp, d /= maxBound]
    go sp [] = sp
    go sp ((n, neighbors) : rest) =
      let
        newSP = foldl' (\acc (m, w) ->
                        let dist = (maybe maxBound id (Map.lookup n acc)) + w
                        in Map.insert m (min (maybe maxBound id (Map.lookup m acc)) dist) acc) sp neighbors
      in go newSP rest
  in go initSP (Map.toList graph)

main :: IO ()
main = do
  graph <- readNodesAndEdges "nodes.txt" "edges.txt"
  let shortestPaths = dijkstra graph "A"
  print graph
  print shortestPaths
