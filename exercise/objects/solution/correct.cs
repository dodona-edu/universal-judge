public class EqualChecker {

    private readonly int number;

    public EqualChecker(int number) {
        this.number = number;
    }

    public bool Check(int other) {
        return this.number == other;
    }
}
