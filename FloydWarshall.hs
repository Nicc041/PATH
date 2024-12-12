module FloydWarshall (floydWarshall, MatrixGraph , Node, Edge, showMatrix) where

import Data.Array
import Data.List (intercalate)

-- Define types for clarity
type Node = Int
type Weight = Int
type Edge = (Node, Node, Weight)
type MatrixGraph  = Array (Node, Node) Weight

-- Define infinity for representing no connection
inf :: Weight
inf = maxBound `div` 2

-- Initialize the graph from a list of edges
initGraph :: Int -> [Edge] -> MatrixGraph 
initGraph n edges =
    let bounds' = ((1, 1), (n, n))
        initialGraph = array bounds' [((i, j), if i == j then 0 else inf) | i <- [1..n], j <- [1..n]]
        formattedEdges = [((src, dst), weight) | (src, dst, weight) <- edges]
    in initialGraph // formattedEdges

-- Floyd-Warshall algorithm with negative-weight cycle detection
floydWarshall :: Int -> [Edge] -> Either String MatrixGraph 
floydWarshall n edges = 
    let
        graph = initGraph n edges
        result = foldl updateDist graph [1..n]
        
        -- Update distances for each node pair
        updateDist g k = array (bounds g) [((i, j), min (g ! (i, j)) (g ! (i, k) + g ! (k, j))) | (i, j) <- indices g]

        -- Check for negative-weight cycles on the diagonal
        hasNegativeCycle g = any (\i -> g ! (i, i) < 0) [1..n]
    in
        if hasNegativeCycle result
        then Left "Graph contains a negative-weight cycle"
        else Right result

-- Function to display the MatrixGraph as a matrix
showMatrix :: [String] -> MatrixGraph -> String
showMatrix nodes graph =
    let ((r1, c1), (r2, c2)) = bounds graph
        header = "    " ++ unwords nodes
        rows = [nodes !! (i - 1) ++ " " ++ unwords [showCell (graph ! (i, j)) | j <- [c1..c2]] | i <- [r1..r2]]
    in unlines (header : rows)
  where
    inf = maxBound `div` 2  -- Same as the infinity value used in the module
    showCell x = if x == inf then "inf" else show x



