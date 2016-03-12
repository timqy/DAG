{-# LANGUAGE FlexibleInstances #-}
module DAG (Graph, addVertex) where
import Data.Char
import Data.List
import Data.Maybe

type Node a = (Int, a)
type Edge a = (Int, Int, a)

data Graph a = Graph {
    nodes :: [Node a],
    edges :: [Edge a]
} deriving (Show, Eq, Read)

class Weight a where
	wcmp :: a -> a -> Bool
	wsum :: a -> a -> a

-- Implementation of Weight as an integer
instance Weight (Integer) where
	wcmp x y = x > y
	wsum x y = x + y

-- Another implementation of Weight
instance Weight ([Char]) where
	wcmp x y = x > y
	wsum x y = x ++ y 

-- v = add_vertex(w)
-- A vertex with the specified weight w is added to the DAG and its unique vertex identifier v is returned.
addVertex :: Graph a -> a -> (Graph a, Int)
addVertex (Graph nodes edges) w = ((Graph ((length nodes, w):nodes) edges), (length nodes))

-- add_edge(a,b,w)
-- An edge from the vertex with vertex identifier a to the vertex with vertex identifier b is added to the DAG with weight w.
--addEdge
addEdge :: Graph a -> Int -> Int -> a -> Graph a
addEdge (Graph nodes edges) n1 n2 w
    |((existNode (Graph nodes edges) n1) /= Nothing)
        && ((existNode (Graph nodes edges) n2) /= Nothing)
        && (not (existEdgeBetween (Graph nodes edges) n2 n1))
        && (not (existEdgeBetween (Graph nodes edges) n1 n2)) = (Graph nodes ((n1, n2, w):edges))
    |otherwise = error "Nodes does not exist or already exist edge between them"

existNode :: Graph a -> Int -> Maybe (Int)
existNode (Graph nodes edges) n = find (==n) [fst t | t <- nodes]

hasIncomingEdge :: Graph a -> Int -> Bool
hasIncomingEdge (Graph nodes edges) n = (length (filter (\(_,b,_) -> b == n) edges)) > 0

existEdgeBetween :: Graph a -> Int -> Int -> Bool
existEdgeBetween (Graph nodes edges) n1 n2 = (length (filter (\(b,c,_) -> b == n1 && c == n2) edges)) > 0

getStartNodes :: Graph a -> [Int]
getStartNodes a = (nub(getAllOutgoing a)) \\ (nub(getAllIncoming a))

getAllOutgoing :: Graph a -> [Int]
getAllOutgoing (Graph nodes edges) = map (getOutgoing) edges

getAllIncoming :: Graph a -> [Int]
getAllIncoming (Graph nodes edges) = map (getIncoming) edges

getOutgoing :: Edge a -> Int
getOutgoing (a,_,_) = a

getIncoming :: Edge a -> Int
getIncoming (_,a,_) = a

getNodeIncoming :: Graph a -> Int -> [Edge a]
getNodeIncoming (Graph nodes edges) nodeID = (filter (\(_,a,_) -> a == nodeID) edges)

getNodeOutgoing :: Graph a -> Int -> [Edge a]
getNodeOutgoing (Graph nodes edges) nodeID = (filter (\(a,_,_) -> a == nodeID) edges)

removeEdgesFromNode :: (Eq a) => Graph a -> Int -> Graph a
removeEdgesFromNode (Graph nodes edges) nodeID = (Graph nodes (edges \\ (getNodeOutgoing (Graph nodes edges) nodeID)))

getAdjacentNodes :: Graph a -> Int -> [Int]
getAdjacentNodes (Graph nodes edges) nodeID = map (getIncoming) (filter (\(a,_,_) -> a == nodeID) edges) 

--Wrapper for topologicalSort 
-- Calls Topological sort with the graph and start nodes.
topologicalOrdering :: (Eq a) => Graph a -> [Int]
topologicalOrdering (Graph nodes edges) = nub(topologicalSort (Graph nodes edges) (getStartNodes (Graph nodes edges)))

--Sorts the DAG in a topological ordering.
topologicalSort :: (Eq a) => Graph a -> [Int] -> [Int]
topologicalSort (Graph nodes edges) [] = [] -- base case (start nodes is empty)
topologicalSort (Graph nodes edges) (head : tail) = head : (topologicalSort (removeEdgesFromNode (Graph nodes edges) head) (tail ++ (getAdjacentNodes (Graph nodes edges) head)))

-- w = weight_of_longest_path(a,b,f,g)
-- Returns the weight w of the longest path which starts at the vertex with vertex identifier a and ends at the vertex with vertex identifier b.
-- The weight of a path is the sum of f(w), where w ranges over all the weights of the vertices in the path, and g(w), where w ranges over all the weights of the edges in the path.
weightOfLongestPath :: Graph a -> Int -> Int -> Int
weightOfLongestPath graph start end = traverseDag graph (getAdjacentNodes graph start) end 0

traverseDag :: Graph a -> [Int] -> Int -> Int -> Int
traverseDag graph (nodehead:nodetail) end sum = 0



--TESTS
g = Graph [] [] -- Empty graph
g1 = addVertex g 4
g2 = addVertex (fst g1) 5
g3 = addVertex (fst g2) 3
g4 = addVertex (fst g3) 2
g5 = addVertex (fst g4) 7

e1 = addEdge (fst g5) 0 1 2
e2 = addEdge e1 0 2 2
e3 = addEdge e2 1 3 1
e4 = addEdge e3 2 3 1
e5 = addEdge e4 2 4 1
e6 = addEdge e5 3 4 3



