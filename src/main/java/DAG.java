import java.util.*;

/**
 * Created by dv13thg on 3/9/16.
 */
public class DAG<W extends Comparable<W>> {

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
        nodeMap.put(nodeID, new Node<W>(nodeID,weight));
        return nodeID;
    }


    /**
     * Adds an edge between two nodes
     * @param fromID Id of the node to link the edge from
     * @param toID ID of the node to add an edge to
     * @param weight weight of the edge
     */
    public void addEdge(int fromID, int toID, W weight){
        Node<W> from = nodeMap.get(fromID);
        Node<W> to = nodeMap.get(toID);

        if(!from.hasOutgoingEdge(to) && !to.hasOutgoingEdge(from)){
            from.addEdge(to,weight);
        }
    }

    /**
     * Using khan's algorithm retrieve the topological ordering of the dag
     * @return a list containing the topological ordering.
     */
    public List<Node<W>> topologicalOrdering(){
        /** find a list of "start nodes" which have no incoming edges and insert them into a set S */
        HashMap<Node, W> removedEdges = new HashMap<>();
        List<Node<W>> sortedList = new ArrayList<>();


        Stack<Node<W>> nodeSet = new Stack<>();
        HashMap<Node<W>,Integer> nodeEdges = retrieveNodeEdges();
        for(Node<W> startNode : nodeEdges.keySet())
            if(nodeEdges.get(startNode) == 0)
                nodeSet.push(startNode);


        while(!nodeSet.isEmpty()){
            Node<W> node = nodeSet.pop();
            sortedList.add(node);

            HashMap<Node, W> edges = node.getEdges();
            for(Node edgeNode : edges.keySet()) {
                nodeEdges.put(edgeNode, nodeEdges.get(edgeNode) - 1);

                if (nodeEdges.get(edgeNode) == 0)
                    nodeSet.push(edgeNode);
            }
        }
        return sortedList;

    }

    /**
     * retrieves all nodes with no incoming edges
     * @return a list of nodes with no incoming edges.
     */
    public HashMap<Node<W>,Integer> retrieveNodeEdges(){
        HashMap<Node<W>,Integer> nodes = new HashMap<>();
        for(Node<W> node : nodeMap.values()){
            nodes.put(node,0);
        }

        for(Node<W> node : nodeMap.values()){
            for(Node edgeNode : node.getEdges().keySet()){
                nodes.put(edgeNode,nodes.get(edgeNode)+1);
            }
        }
        return nodes;
    }

    public W weightOfLongestPath(Node<W> start, Node<W> end){
        List<Node<W>> topological = topologicalOrdering();

        for each vertex v ∈ V in linearized order
        do dist(v) = max(u,v)∈E {dist(u) + w(u, v)}
        return maxv∈V {dist(v)}
        return null;
    }


}
