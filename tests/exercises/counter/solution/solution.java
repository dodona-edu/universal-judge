public class Counter {
    private int counter;

    public Counter() {
        counter = 0;
    }

    public Counter add() {
        counter++;
        return this;
    }

    public int get() {
        return counter;
    }
}
