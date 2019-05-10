/**
 *
 * @author ntaff
 */
public class ValueSE extends StackElem{
    private final Value val;

    public ValueSE(Value val){
        this.val = val;
    }

    public Value get_val() {
        return val;
    }

}
