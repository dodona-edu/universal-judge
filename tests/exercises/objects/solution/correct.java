import java.util.*;

class EqualChecker {

    private final int number;

    EqualChecker(int number) {
        this.number = number;
    }

    public boolean check(int other) {
        return this.number == other;
    }

    public static Set<List<Integer>> setTest() {
        return Set.of(List.of(1, 2), List.of(2, 3));
    }
}
