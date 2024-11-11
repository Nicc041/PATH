import Data.Char (isSpace)
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)

-- Represent a node as a String
type Node = String

-- Represent an edge as a triple of source, destination, and weight
type Edge = (Node, Node, Int)

-- Represent the graph as a list of edges
type Graph = [Edge]

-- Represent the shortest path information for each node
type ShortestPath = [(Node, Int)]

-- Parse a line of nodes file
parseNode :: String -> Node
parseNode = dropWhile isSpace

-- Parse a line of edges file
parseEdge :: String -> Edge
parseEdge line =
  let [src, dst, w] = words line
  in (src, dst, read w)

-- Read nodes and edges from files
readNodesAndEdges :: FilePath -> FilePath -> IO (Graph)
readNodesAndEdges nodeFile edgeFile = do
  nodes <- lines <$> readFile nodeFile
  edges <- map parseEdge . lines <$> readFile edgeFile
  return edges

-- Dijkstra's algorithm
dijkstra :: Graph -> Node -> ShortestPath
dijkstra graph start =
  let
    -- Initialize the shortest path information
    initSP = [(n, if n == start then 0 else maxBound) | n <- nub (concatMap (\(s, d, _) -> [s, d]) graph)]
    -- Helper function to find the next node to visit
    nextNode sp = minimumBy (comparing snd) [(n, d) | (n, d) <- sp, d /= maxBound]
    -- Dijkstra's algorithm implementation
    go sp [] = sp
    go sp ((s, d, w):es) =
      let
        newDist = snd (head (filter ((== s) . fst) sp)) + w
        sp' = [(if n == d then d else n, min (snd (head (filter ((== n) . fst) sp))) newDist) | (n, _) <- sp]
      in go sp' es
  in go initSP graph

main :: IO ()
main = do
  graph <- readNodesAndEdges "nodes.txt" "edges.txt"
  let shortestPaths = dijkstra graph "A"
  print shortestPaths
