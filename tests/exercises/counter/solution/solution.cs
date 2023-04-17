public class Counter {
    private int counter;

    public Counter() {
        counter = 0;
    }

    public Counter Add() {
        counter++;
        return this;
    }

    public int Get() {
        return counter;
    }
}
