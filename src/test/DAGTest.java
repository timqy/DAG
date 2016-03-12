
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

        dagtest.addVertex(4);
        dagtest.addVertex(5);
        dagtest.addVertex(3);
        dagtest.addVertex(2);
        dagtest.addVertex(7);

        dagtest.addEdge(1,2,2);
        dagtest.addEdge(1,3,2);
        dagtest.addEdge(2,4,1);
        dagtest.addEdge(3,4,1);
        dagtest.addEdge(3,5,1);
        dagtest.addEdge(4,5,3);
    }

    @Test
    public void testTopologicalOrdering() throws Exception {
        List<Node<Integer>> nodelist = dagtest.topologicalOrdering();
        for(Node node : nodelist){
            System.out.println("[" + node.getID() + "] Node = " + node.getWeight());
        }
    }

    @Test
    public void testFindStartNodes() throws Exception {
        HashMap<Node<Integer>, Integer> map = dagtest.incomingEdges();

        assertTrue(map.containsValue(0));
        assertTrue(map.containsValue(1));

        for(Node n : map.keySet()){
            System.out.println("node ["+n.getID()+"]: " + map.get(n));
            assertTrue(map.get(n) == 1 || map.get(n) == 0 || map.get(n) == 2);
        }
    }

    @Test
    public void testLongestPath(){
        assertEquals((int)dagtest.weightOfLongestPath(1,5,new intOp()),24);
    }

    @Test
    public void TestStringWeight(){
        DAG StringDag = new DAG<String>();

        StringDag.addVertex("4");
        StringDag.addVertex("5");
        StringDag.addVertex("3");
        StringDag.addVertex("2");
        StringDag.addVertex("7");

        StringDag.addEdge(1,2,"2");
        StringDag.addEdge(1,3,"2");
        StringDag.addEdge(2,4,"1");
        StringDag.addEdge(3,4,"1");
        StringDag.addEdge(3,5,"1");
        StringDag.addEdge(4,5,"3");

        assertEquals((int)dagtest.weightOfLongestPath(1,5,new intOp()),24);
    }
}