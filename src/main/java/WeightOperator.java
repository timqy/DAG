/**
 * Weightoperator
 *
 */
public interface WeightOperator <W>{

    /**
     * used to manipulate either edges or nodes weight in
     * weight of longest path
     * @param a the weight of node or edge
     * @return the processed a.
     */
    public W operate(W a);

}
