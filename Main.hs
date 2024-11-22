import Dijkstra (dijkstra)
import BellmanFord (bellmanFord)
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Data.List (foldl')

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

formatDistance :: Maybe Int -> String
formatDistance Nothing = "no path"
formatDistance (Just d) = show d

main :: IO ()
main = do
  -- Ask user for input files
  putStrLn "Enter the file path for the nodes file:"
  nodeFile <- getLine
  putStrLn "Enter the file path for the edges file:"
  edgeFile <- getLine
  
  -- Load graph
  putStrLn "\nLoading graph..."
  graph <- readNodesAndEdges nodeFile edgeFile
  let nodes = Map.keys graph
  
  -- Display available nodes
  putStrLn "\nAvailable nodes in the graph:"
  mapM_ putStrLn nodes

  -- Prompt user for starting node
  putStrLn "\nEnter the starting node:"
  startNode <- getLine
  
  -- Validate starting node
  if startNode `notElem` nodes
    then putStrLn "Invalid starting node. Exiting..."
    else do
      -- Prompt user for algorithm choice
      putStrLn "\nChoose algorithm:"
      putStrLn "1. Dijkstra's Algorithm"
      putStrLn "2. Bellman-Ford Algorithm"
      choice <- getLine
      
      case choice of
        "1" -> do
          putStrLn "\nRunning Dijkstra's Algorithm..."
          let shortestPaths = dijkstra graph startNode
          putStrLn "Shortest paths:"
          mapM_ (\(node, dist) -> 
            putStrLn $ node ++ ": " ++ formatDistance dist) 
            (Map.toList shortestPaths)
        
        "2" -> do
          putStrLn "\nRunning Bellman-Ford Algorithm..."
          case bellmanFord graph startNode of
            Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
            Right shortestPaths -> do
              putStrLn "Shortest paths:"
              mapM_ (\(node, dist) -> 
                putStrLn $ node ++ ": " ++ formatDistance dist) 
                (Map.toList shortestPaths)
        
        _ -> putStrLn "Invalid choice. Please select 1 or 2."