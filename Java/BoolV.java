public class BoolV implements Value {

	private boolean val;

	public BoolV(boolean val) {
		this.val = val;
	}

	public boolean getValue() {
		return val;
	}

	@Override
	public String toString() {
		return String.valueOf(val);
	}
}
