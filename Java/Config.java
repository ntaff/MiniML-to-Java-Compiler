import java.util.*;

public class Config {
	private Value val;
	private List<Instr> code;
	private List<StackElem> stack;

	public Config (Value val, List<Instr> code, List<StackElem> stack) {
		this.val = val;
		this.code = code;
		this.stack = stack;
	}

	public Value getValue() {
		return val;
	}

	public void setValue(Value val) {
		this.value = val;
	}

	List<Instr> getCode() {
		return code;
	}

	public void setCode(List<Instr> code) {
		this.code = code;
	}

	List<StackElem> getStack() {
		return stack;
	}

	public void setStack(List<StackElem> stack) {
		this.stack = stack;
	}

	public void nextInstruction() {
		code.remove(0);
	}

	// one-step execution
	public boolean exec_step() {
    if (code.isEmpty()){
         return false;
     }
     else{
         code.get(0).exec_instr(this);
         return true;
}
	}

	// run to completion
	public void exec() {
    while (exec_step()){}
}

// run for n steps
void step(int n) {
for(int i=0; i<n && exec_step(); n++){}
}
	}
}
