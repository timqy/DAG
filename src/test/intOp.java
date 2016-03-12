/**
 * Created by dv13thg on 3/12/16.
 */
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
