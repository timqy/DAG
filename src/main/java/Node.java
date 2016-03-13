import java.util.HashMap;

/**
 *
 */
public class Node<W> {

    private int ID;
    private W weight;
    private HashMap<Node, W> edges;

    public Node(int ID, W weight) {
        this.ID = ID;
        this.weight = weight;
        edges = new HashMap<>();
    }

    public int getID() {
        return ID;
    }

    public W getWeight() {
        return weight;
    }

    /**
     * @param obj The object to compare against
     * @return Boolean if they have the same id.
     */
    @Override
    public boolean equals(Object obj){
        if(obj.getClass() == Node.class)
            return ((Node) obj).getID() == this.ID;
        return false;
    }

    /**
     * Adds an edge between this node and another
     * @param node the node to be connected
     * @param weight the weight of the edge
     */
    public void addEdge(Node node, W weight){
        edges.put(node,weight);
    }

    /**
     * checks whether this node is connected to another node
     * @param node node to check against
     * @return boolean for if the node is connected
     */
    public boolean hasOutgoingEdge(Node node){
        return edges.containsKey(node);
    }

    public HashMap<Node, W> getEdges() {
        return edges;
    }
}
