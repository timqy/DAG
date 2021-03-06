import java.security.InvalidParameterException;
import java.util.*;

/**
 * A directed acyclic graph with nodes and edges with the weight W.
 *
 */
public class DAG<W> {

    private HashMap<Integer,Node<W>> nodeMap;
    private int nodeID;

    public DAG() {
        this.nodeMap = new HashMap<>();
        this.nodeID = 0;
    }

    /**
     * Increments the current nodeID's and assigns it to a
     * new Node with the input weight.
     * @param weight weight of the node
     * @return the id of the node
     */
    public int addVertex(W weight){
        nodeID++;
        nodeMap.put(nodeID, new Node<>(nodeID,weight));
        return nodeID;
    }


    /**
     * Adds an edge between two nodes
     * @param fromID Id of the node to link the edge from
     * @param toID ID of the node to add an edge to
     * @param weight weight of the edge
     */
    public void addEdge(int fromID, int toID, W weight) throws InvalidParameterException{
        Node<W> from = nodeMap.get(fromID);
        Node<W> to = nodeMap.get(toID);

        if(!from.hasOutgoingEdge(to) && !to.hasOutgoingEdge(from)){
            from.addEdge(to,weight);
            if(hasCycles()){
                from.getEdges().remove(to);
                throw new InvalidParameterException("Invalid Edge; creates a cycle");
            }
        }
    }

    /**
     * Using khan's algorithm to retrieve the topological ordering of the
     * directed acyclic graph as a list of nodes.
     * @return a list of nodes in topological order.
     */
    public List<Node<W>> topologicalOrdering(){
        HashMap<Node<W>,Integer> nodeIncomingEdges = incomingEdges();
        List<Node<W>> sortedList = new ArrayList<>();
        Stack<Node<W>> nodeSet = new Stack<>();

        for(Node<W> startNode : nodeIncomingEdges.keySet())
            System.out.println("startnode " + startNode.getID() + " edges " + nodeIncomingEdges.get(startNode));

        for(Node<W> startNode : nodeIncomingEdges.keySet())
            if(nodeIncomingEdges.get(startNode) == 0)
                nodeSet.push(startNode);

        while(!nodeSet.isEmpty()){
            Node<W> node = nodeSet.pop();
            sortedList.add(node);

            HashMap<Node, W> edges = node.getEdges();
            for(Node<W> edgeNode : edges.keySet()) {
                nodeIncomingEdges.put(edgeNode, nodeIncomingEdges.get(edgeNode) - 1);

                if (nodeIncomingEdges.get(edgeNode) == 0)
                    nodeSet.push(edgeNode);

            }
        }
        return sortedList;
    }

    /**
     * Reduces all startnodes until there is no left.
     * If no node is left in the DAG, there is no cycle.
     * if there is nodes still left in the dag a cycle excists.
     * @return boolean
     */
    private boolean hasCycles(){
        HashMap<Node<W>,Integer> startNodes = incomingEdges();
        boolean isDone = false;

        while(!isDone) {
            isDone = true;
            Set<Node<W>> startNodesKeySet = startNodes.keySet();
            Iterator<Map.Entry<Node<W>, Integer>> startnodeit = startNodes.entrySet().iterator();

            while(startnodeit.hasNext()){
                Map.Entry<Node<W>, Integer> node = startnodeit.next();

                if (startNodes.get(node.getKey()) == 0) {
                    for (Node<W> edgenode : node.getKey().getEdges().keySet()) {
                        startNodes.put(edgenode, startNodes.get(edgenode) - 1);
                        isDone = false;
                    }
                    startnodeit.remove();
                }
            }
        }
        return !startNodes.isEmpty();
    }

    /**
     * Creates a list of nodes with their number of incoming
     * edges
     * @return a list of nodes with no incoming edges.
     */
    private HashMap<Node<W>,Integer> incomingEdges(){
        HashMap<Node<W>,Integer> nodes = new HashMap<>();

        /** Set all incoming edges to 0 */
        for(Node<W> node : nodeMap.values())
            nodes.put(node,0);

        /** increment the value of each node with the number of incoming edges */
        for(Node<W> node : nodeMap.values()){
            for(Node edgeNode : node.getEdges().keySet()){
                nodes.put(edgeNode,nodes.get(edgeNode)+1);
            }
        }
        return nodes;
    }

    /**
     * Retrieves the length of the longest path by using breadth first.
     * @param start the start node iD
     * @param end end node ID
     * @param operator the operator to use with the weight
     * @param f the weightoperator for nodes
     * @param g the weightoperator for edges
     * @return the highest value
     */
    public W weightOfLongestPath(int start, int end, Operator operator, WeightOperator<W> f, WeightOperator<W> g){
        HashMap<Node<W>,W> visited = new HashMap<>();
        Stack<Node<W>> nodes = new Stack<>();
        nodes.push(nodeMap.get(start));
        visited.put(nodeMap.get(start), nodeMap.get(start).getWeight());

        while(!nodes.isEmpty()){
            Node<W> current = nodes.pop();
            W currentLength = visited.get(current);

            for (Node<W> adjacentNode : current.getEdges().keySet()){
                /** This weight + the weight of the edge + the weight of the new node */
                W adjLength = (W)operator.sum(currentLength,
                                                operator.sum(
                                                            (f.operate(adjacentNode.getWeight())),
                                                            (g.operate(current.getEdges().get(adjacentNode)))));

                if(visited.containsKey(adjacentNode)){
                    if(operator.compare(adjLength,visited.get(adjacentNode))){
                        visited.put(adjacentNode,adjLength);
                        nodes.push(adjacentNode);
                    }
                } else {
                    nodes.push(adjacentNode);
                    visited.put(adjacentNode,adjLength);
                }
            }
        }
        return visited.get(nodeMap.get(end));
    }
}
