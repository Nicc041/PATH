import GraphParser
import Dijkstra (dijkstra)
import BellmanFord (bellmanFord)
import FloydWarshall (floydWarshall, showMatrix)
import qualified Data.Map.Strict as Map
import CampusMapParser (campusReadNodesAndEdges)
import System.FilePath (takeExtension)

-- Format distance for output
formatDistance :: Maybe Int -> String
formatDistance Nothing = "no path"
formatDistance (Just d) = show d

-- Check for negative weights in the graph
hasNegativeEdges :: Graph -> Bool
hasNegativeEdges graph = any (\(_, weight) -> weight < 0) (concat (Map.elems graph))

main :: IO ()
main = do
  -- Ask user for input files
  putStrLn "\nEnter the node file path (CSV or TXT):"
  nodeFile <- getLine
  putStrLn "\nEnter the edge file path (CSV or TXT):"
  edgeFile <- getLine
  
  -- Choose parser based on file extension
  let ext = takeExtension nodeFile
  graph <- case ext of
    ".csv" -> campusReadNodesAndEdges nodeFile edgeFile
    ".txt" -> readNodesAndEdges nodeFile edgeFile
    _      -> error "Unsupported file format. Please use a .csv or .txt file."

  -- Prompt user for algorithm choice
  putStrLn "\nChoose algorithm:"
  putStrLn "1. Dijkstra's Algorithm"
  putStrLn "2. Bellman-Ford Algorithm"
  putStrLn "3. Floyd-Warshall Algorithm"
  choice <- getLine

  case choice of
    "1" -> do
      -- Display available nodes for Dijkstra
      let nodes = Map.keys graph
      putStrLn "\nAvailable nodes in the graph:"
      mapM_ putStrLn nodes

      -- Prompt user for starting node
      putStrLn "\nEnter the starting node:"
      startNode <- getLine
      if startNode `notElem` nodes
        then putStrLn "Invalid starting node. Exiting..."
        else if hasNegativeEdges graph
          then putStrLn "Error: Dijkstra's algorithm cannot be used with negative edge weights."
          else do
            putStrLn "\nRunning Dijkstra's Algorithm..."
            let shortestPaths = dijkstra graph startNode
            putStrLn "Shortest paths:"
            mapM_ (\(node, dist) -> putStrLn $ node ++ ": " ++ formatDistance dist) (Map.toList shortestPaths)

    "2" -> do
      -- Display available nodes for Bellman-Ford
      let nodes = Map.keys graph
      putStrLn "\nAvailable nodes in the graph:"
      mapM_ putStrLn nodes

      -- Prompt user for starting node
      putStrLn "\nEnter the starting node:"
      startNode <- getLine
      if startNode `notElem` nodes
        then putStrLn "Invalid starting node. Exiting..."
        else do
          putStrLn "\nRunning Bellman-Ford Algorithm..."
          case bellmanFord graph startNode of
            Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
            Right shortestPaths -> do
              putStrLn "Shortest paths:"
              mapM_ (\(node, dist) -> putStrLn $ node ++ ": " ++ formatDistance dist) (Map.toList shortestPaths)

    "3" -> do
      putStrLn "\nRunning Floyd-Warshall Algorithm..."
      let nodes = Map.keys graph
          edgesList = concatMap (\(src, dests) -> [(src, dst, weight) | (dst, weight) <- dests]) (Map.toList graph)
          numNodes = length nodes
          nodeIndexMap = zip nodes [1..]
          indexedEdges = map (\(s, d, w) -> (lookupIndex s, lookupIndex d, w)) edgesList
            where lookupIndex n = case lookup n nodeIndexMap of
                                    Just idx -> idx
                                    Nothing  -> error "Node not found"
      case floydWarshall numNodes indexedEdges of
        Left err -> putStrLn err
        Right result -> putStrLn $ showMatrix nodes result


    _ -> putStrLn "Invalid choice. Please select 1, 2, or 3."
