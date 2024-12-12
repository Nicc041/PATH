Run the code in ghci by using 
```ghci Main.hs```
This will initiate I/O that will allow you to interact with the parts of the project that are currently working
All of the libraries we are using are pretty standard
Structure:

Graph Parsing Modules
GraphParser.hs:
Purpose: Parses nodes and edges from input files to construct a graph.
CampusMapParser.hs​:
Purpose: Parses nodes and edges specifically for campus map data, including location coordinates and calculating distances.
Shortest Path Algorithms
Dijkstra.hs:
Purpose: Implements Dijkstra's algorithm for finding shortest paths in graphs with non-negative weights.
BellmanFord.hs​:
Purpose: Implements the Bellman-Ford algorithm for graphs that may contain negative edge weights.
FloydWarshall.hs​:
Purpose: Implements the Floyd-Warshall algorithm for finding shortest paths between all pairs of nodes.
Main Execution Module
Main.hs​:
Purpose: Acts as the entry point for the program, handling user input, invoking appropriate parsers and algorithms, and displaying results.
