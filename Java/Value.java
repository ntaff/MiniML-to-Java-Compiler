public interface Value {
	default void print() {
		System.out.println(this.toString());
	}
}
