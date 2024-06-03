public class EqualChecker {

    private readonly int number;
    public int Prop = 0;

    public EqualChecker(int number) {
        this.number = number;
    }

    public bool Check(int other) {
        return this.number == other;
    }
}
