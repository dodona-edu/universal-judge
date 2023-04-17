class EqualChecker {

    private final int number;

    EqualChecker(int number) {
        this.number = number;
    }

    public boolean check(int other) {
        return this.number == other;
    }
}