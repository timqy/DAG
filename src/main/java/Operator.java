/**
 * An interface operator for the user to implement its
 * own summation of two objects and comparison.
 */
public interface Operator {

    /**
     * compare the two objects and to see if a is greater than b in some sense.
     * @param a the first object to compare to
     * @param b the second object to compare to
     * @return a boolean for if a is greater than b
     */
    public boolean compare(Object a,Object b);

    /**
     * Use summation on the two objects
     * @param a first object
     * @param b second object
     * @return the sum of the two objects
     */
    public Object sum(Object a, Object b);
}
