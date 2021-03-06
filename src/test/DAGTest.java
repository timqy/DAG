
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.security.InvalidParameterException;
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
        System.out.print("TopologicalOrdering [id,weight]: ");
        for(Node node : nodelist){
            System.out.print("["+node.getID() + ","+node.getWeight()+"]");
        }
        System.out.println("");
    }

    @Test
    public void testLongestPath(){
        WeightOperator<Integer> wp = new WeightOperator<Integer>() {
            @Override
            public Integer operate(Integer a) {
                return a;
            }
        };
        int weightoflongest = (int)dagtest.weightOfLongestPath(1,5,new intOp(),wp,wp);
        assertEquals(weightoflongest,24);
        System.out.println("Weight of longest path string DAG : " + weightoflongest);
    }

    @Test (expected=InvalidParameterException.class)
    public void testCyclicDAG(){
        DAG<Integer> cyc = new DAG<Integer>();;


        cyc.addVertex(4);
        cyc.addVertex(5);
        cyc.addVertex(3);

        cyc.addEdge(1,2,2);
        cyc.addEdge(2,3,1);

        /** edge node */
        cyc.addVertex(5);
        cyc.addEdge(4,3,1);

        /** failing edge */
        cyc.addEdge(3,1,1);
    }


    @Test
    public void TestStringWeight(){
        DAG StringDag = new DAG<String>();

        StringDag.addVertex("e");
        StringDag.addVertex("d");
        StringDag.addVertex("c");
        StringDag.addVertex("b");
        StringDag.addVertex("g");

        StringDag.addEdge(1,2,"b");
        StringDag.addEdge(1,3,"b");
        StringDag.addEdge(2,4,"a");
        StringDag.addEdge(3,4,"a");
        StringDag.addEdge(3,5,"a");
        StringDag.addEdge(4,5,"c");

        WeightOperator<String> wp = new WeightOperator<String>() {
            @Override
            public String operate(String a) {
                return a;
            }
        };

        System.out.println("Weight of longest path string DAG : " +
                            StringDag.weightOfLongestPath(1,5,new StringOp(),wp,wp));
    }
    public class intOp implements Operator{

        @Override
        public boolean compare(Object a, Object b) {
            return (int)a > (int)b;
        }

        @Override
        public Object sum(Object a, Object b) {
            return (int)a + (int)b;
        }
    }

    public class StringOp implements Operator{

        @Override
        public boolean compare(Object a, Object b) {
            return ((String)a).compareTo((String)b) > 0;
        }

        @Override
        public Object sum(Object a, Object b) {
            return (String)a + (String)b;
        }
    }

}

