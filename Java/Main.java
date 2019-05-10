// compile with: javac *java
// run with: java Main

import java.util.*;

public class Main {

    public static void main(String[] args){

        LinkedList<Integer> ll1, ll2;
        LinkedList<Instr> example_code ;
        Config conf;
        conf = new Config(new NullV(), Gen.code, LLE.empty());
        conf.exec();
        conf.get_value().print_value();

    }

}
