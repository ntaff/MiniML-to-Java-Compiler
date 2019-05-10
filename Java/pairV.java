public class PairV implements Value {
	private Value fst;
	private Value snd;

	public PairV(Value fst, Value snd) {
		this.fst = fst;
		this.snd = snd;
	}

	public Value getFst() {
		return fst;
	}

	public Value getSnd() {
		return snd;
	}

	@Override
	public String toString() {
		return "(" + fst.toString() + ", " + snd.toString() + ")";
	}
}
