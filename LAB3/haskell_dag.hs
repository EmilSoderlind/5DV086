import Data.List

type Weight w = w
data Vertex w = Vertex Integer (Weight w) deriving (Show)
data Edge w = Edge (Vertex w) (Vertex w) (Weight w) deriving (Show)
data DAG w = DAG [Edge w] [Vertex w] deriving (Show)

-- basic functions to test weight_of_longest_path
f a = a 
g a = a

-- Finds the longest path in given DAG between two vertices
weight_of_longest_path :: (Ord w, Num w) => Integer -> Integer -> DAG w -> (w -> w) -> (w -> w)-> Weight w
weight_of_longest_path id1 id2 (DAG edgelist vertexlist) f g 
 | id1 == id2 = f (get_vertex_weight vertexlist id1) 
 | otherwise = 0 --maximum $ map ( \ x -> f (get_vertex_weight vertexlist id1) + g (get_edge_weight edgelist id1 id2 + weight_of_longest_path DAG id1 x f g))

-- To be used in longest path (not sure if right)
--get_edge_weight :: (Ord w, Num w) => [Edge w] -> Integer -> Integer -> Weight w
--get_edge_weight (x:xs) id1 id2
-- | fst getVertecesIDs x == id1 && snd getVertecesIDs x == id2 = getWeightEdge x
-- | otherwise = get_edge_weight xs id1 id2

-- Given a vertex ID, returns its weight (not sure if right)
get_vertex_weight :: (Ord w, Num w) => [Vertex w] -> Integer -> Weight w
get_vertex_weight (x:xs) id 
 | getVertexID x == id = getWeight x
 | otherwise = get_vertex_weight xs id

-- Returns a vertex weight
getWeight :: (Ord w, Num w) => Vertex w -> Weight w
getWeight (Vertex _ w) = w

getWeightEdge :: (Ord w, Num w) => Edge w -> Weight w
getWeightEdge (Edge _ _ w) = w

-- Creates a node with weight w
add_vertex :: (Ord w, Num w) => DAG w -> Weight w -> (Integer, DAG w)
add_vertex (DAG edgelist vertexlist) w 
 | length vertexlist == 0 = (1, DAG edgelist [Vertex 1 w])
 | otherwise = (getNextID(last vertexlist), (DAG edgelist (vertexlist ++ [Vertex (getNextID(last vertexlist)) w])))

-- Return succeding free ID
getNextID :: (Ord w, Num w) => Vertex w -> Integer
getNextID x = succ (getVertexID x)

-- Creates an edge between verteces a and b, with weight w
add_edge ::(Ord w, Num w) => Vertex w -> Vertex w -> Weight w -> DAG w -> DAG w
add_edge a b w (DAG edgelist vertexlist) 
 | not (isEdge a b edgelist)  = DAG (Edge a b w : edgelist) vertexlist
 | otherwise = DAG edgelist vertexlist --returns unchanged edgelist

--incoming_verteces :: (Ord w, Num w) => [Vertex w] -> [Edge w] -> [Vertex w]
--incoming_verteces x:xs a:bs
-- | length xs == 0 = []
-- | getVertexID x == snd getVertecesIDs a
 
-- Computes a topological ordering of the DAG (verteces from edges + single verteces)
topological_ordering :: (Ord w, Num w) => DAG w -> [Integer]
topological_ordering (DAG edgelist vertexlist) 
 | length vertexlist == 0 = []
 | length edgelist == 0 = map (getVertexID) vertexlist
 | otherwise = nub(removeAndReturnID (sortBy (mostLeft) edgelist) ++ (map (getVertexID) vertexlist)) 

-- Orders vertex accoding to tree
mostLeft :: (Ord w, Num w) => Edge w -> Edge w -> Ordering
mostLeft (Edge v1 _ _) (Edge _ v2 _)
 | getVertexID v1 == getVertexID v2 = GT
 | otherwise = EQ

-- Removes duplicates and returns list of ID
removeAndReturnID :: (Ord w, Num w) => [Edge w] -> [Integer]
removeAndReturnID x = nub (concatMap (\(x,y) -> [x,y]) (map (getVertecesIDs) x))

getVertecesIDs :: (Ord w, Num w) => Edge w -> (Integer, Integer)
getVertecesIDs x = ((getVertexID(getLeftVertex x)), (getVertexID(getRightVertex x)))

-- Checks if an edge already exists between two vertices
-- and also detects if the edge will create a cycle 
isEdge :: (Ord w, Num w) => Vertex w -> Vertex w -> [Edge w] -> Bool
isEdge v1 v2 x 
 | length(filter (compareVertex v1 v2) x) == 0 = False
 | otherwise = True

-- Compares the two vertices with and edge to see if an edge already exists
-- or if a edge in opposite direction exists
compareVertex :: (Ord w, Num w) => Vertex w -> Vertex w -> Edge w -> Bool
compareVertex v1 v2 e 
 | getVertexID v1 == getVertexID(getLeftVertex e) && getVertexID v2 == getVertexID(getRightVertex e) = True -- edge in same order as input
 | getVertexID v2 == getVertexID(getLeftVertex e) && getVertexID v1 == getVertexID(getRightVertex e) = True -- edge in opposite order, creates cycle
 | otherwise = False

-- Returns the left vertex
getLeftVertex :: (Ord w, Num w) => Edge  w -> Vertex w
getLeftVertex (Edge v _ _) = v

-- Returns the right vertex
getRightVertex :: (Ord w, Num w) => Edge  w -> Vertex w
getRightVertex (Edge _ v _) = v
 
-- Returns the ID of a vertex
getVertexID :: (Ord w, Num w) => Vertex w -> Integer
getVertexID (Vertex c _) = c
