import Data.Char (isSpace)
import Data.List (foldl', minimumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Maybe (isJust, fromJust, isNothing)

type Node = String
type Edge = (Node, Node, Int)
type Graph = Map.Map Node [(Node, Int)]

parseNode :: String -> Node
parseNode = dropWhile isSpace

parseEdge :: String -> Edge
parseEdge line =
  let [src, dst, w] = words line
  in (src, dst, read w)

readNodesAndEdges :: FilePath -> FilePath -> IO Graph
readNodesAndEdges nodeFile edgeFile = do
  nodes <- lines <$> readFile nodeFile
  edges <- map parseEdge . lines <$> readFile edgeFile
  return $ foldl' addEdge (Map.fromList [(n, []) | n <- nodes]) edges
  where
    addEdge :: Graph -> Edge -> Graph
    addEdge g (s, d, w) = Map.update (Just . ((d, w) :)) s g

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

formatDistance :: Maybe Int -> String
formatDistance Nothing = "no path"
formatDistance (Just d) = show d

main :: IO ()
main = do
  graph <- readNodesAndEdges "nodes2.txt" "edges2.txt"
  let shortestPaths = dijkstra graph "S"
  putStrLn "Graph structure:"
  print graph
  putStrLn "\nShortest paths from B:"
  mapM_ (\(node, dist) -> 
    putStrLn $ node ++ ": " ++ formatDistance dist) 
    (Map.toList shortestPaths)
