import java.util.*;
class Submission {
    public static List<Integer> numbers() { return new ArrayList<>(List.of(1, 2, 3)); }
    public static int total(List<Integer> xs) { int s = 0; for (int x : xs) s += x; return s; }
}
