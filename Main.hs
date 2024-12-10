import GraphParser
import Dijkstra (dijkstra)
import BellmanFord (bellmanFord)
import qualified Data.Map.Strict as Map
import CampusMapParser(campusReadNodesAndEdges)
import System.FilePath (takeExtension)

-- Format distance for output
formatDistance :: Maybe Int -> String
formatDistance Nothing = "no path"
formatDistance (Just d) = show d

-- Check for negative weights in the graph
hasNegativeEdges :: Graph -> Bool
hasNegativeEdges graph = any (\(_, weight) -> weight < 0) (concat (Map.elems graph))

-- findCampusGraphPath :: IO()
-- findCampusGraphPath = do
--   putStrLn "\nLoading graph..."
--   graph <- campusReadNodesAndEdges "RPI_map_data_Nodes.csv" "RPI_map_data_Edges.csv"
--   let nodes = Map.keys graph
  
  -- Display available nodes
--   putStrLn "\nAvailable nodes in the graph:"
--   mapM_ putStrLn nodes
  
  -- Prompt user for starting node
--   putStrLn "\nEnter the starting node:"
--   startNode <- getLine
--   if startNode `notElem` nodes
--     then putStrLn "Invalid starting node. Exiting..."
--     else do
      -- Prompt user for algorithm choice
--       putStrLn "\nChoose algorithm:"
--       putStrLn "1. Dijkstra's Algorithm"
--       putStrLn "2. Bellman-Ford Algorithm"
--       choice <- getLine
      
--       case choice of
--         "1" -> do
--           if hasNegativeEdges graph
--             then putStrLn "Error: Dijkstra's algorithm cannot be used with negative edge weights."
--             else do
--               putStrLn "\nRunning Dijkstra's Algorithm..."
--               let shortestPaths = dijkstra graph startNode
--               putStrLn "Shortest paths:"
--               mapM_ (\(node, dist) -> 
--                 putStrLn $ node ++ ": " ++ formatDistance dist) 
--                 (Map.toList shortestPaths)
        
--         "2" -> do
--           putStrLn "\nRunning Bellman-Ford Algorithm..."
--           case bellmanFord graph startNode of
--             Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
--             Right shortestPaths -> do
--               putStrLn "Shortest paths:"
--               mapM_ (\(node, dist) -> 
--                 putStrLn $ node ++ ": " ++ formatDistance dist) 
--                 (Map.toList shortestPaths)
--         
--         _ -> putStrLn "Invalid choice. Please select 1 or 2."
  

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
          if hasNegativeEdges graph
            then putStrLn "Error: Dijkstra's algorithm cannot be used with negative edge weights."
            else do
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
