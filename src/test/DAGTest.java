
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Tests DAG
 */
public class DAGTest {

    DAG<Integer> dagtest;

    @Before
    public void setUp() throws Exception {
        dagtest = new DAG<Integer>();

        for(int i = 0; i < 10;i++){
            dagtest.addVertex(i);
        }
    }

    @After
    public void tearDown() throws Exception {

    }

    @Test
    public void testTopologicalOrdering() throws Exception {
        for(int i = 1; i <= 9; i++)
            dagtest.addEdge(i,i+1,2);

        List<Node<Integer>> nodelist = dagtest.topologicalOrdering();
        for(Node node : nodelist){
            System.out.println("[" + node.getID() + "] Node = " + node.getWeight());
        }
    }

    @Test
    public void testFindStartNodes() throws Exception {
        for(int i = 1; i <= 9; i++){
            dagtest.addEdge(i,i+1,2);
        }

        HashMap<Node<Integer>, Integer> map = dagtest.incomingEdges();

        assertTrue(map.containsValue(0));
        assertTrue(map.containsValue(1));

        for(Node n : map.keySet()){
            assertTrue(map.get(n) == 1 || map.get(n) == 0);
        }
    }
}