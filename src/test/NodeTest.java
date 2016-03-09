import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test node functions
 */
public class NodeTest {

    Node<Integer> node;

    @Before
    public void setUp() throws Exception {
        node = new Node<>(0,10);
    }

    @After
    public void tearDown() throws Exception {

    }

    @Test
    public void testGetID() throws Exception {
        assertEquals(0,node.getID());
    }

    @Test
    public void testGetWeight() throws Exception {
        assertEquals(10,(int)node.getWeight());
    }

    @Test
    public void testEquals() throws Exception {
        assertTrue(node.equals(new Node<Integer>(0,10)));
    }

    @Test
    public void testAddEdge() throws Exception {
        Node<Integer> tempNode = new Node<Integer>(1,9);
        node.addEdge(tempNode,2);
        assertTrue(!node.getEdges().isEmpty());
        assertTrue(node.getEdges().containsKey(tempNode));
        assertTrue(node.getEdges().containsValue(2));
    }

    @Test
    public void testHasOutgoingEdge() throws Exception {
        Node<Integer> tempNode = new Node<Integer>(1,9);
        node.addEdge(tempNode,2);
        assertTrue(node.hasOutgoingEdge(tempNode));
    }

    @Test
    public void testGetEdges() throws Exception {
        Node<Integer> tempNode = new Node<Integer>(1,9);

        assertTrue(node.getEdges().isEmpty());

        node.addEdge(tempNode,2);
        assertTrue(!node.getEdges().isEmpty());
    }
}