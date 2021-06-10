import java.util.function.BinaryOperator;
import java.util.function.BiFunction;

public class Store {
    private final int a;
    private final int b;

    public Store(int a, int b) {
        this.a = a;
        this.b = b;
    }

    public int apply(BinaryOperator<Integer> operator) {
        return operator.apply(a, b);
    }

    public static int add(int a, int b) {
        return a + b;
    }

    public static Store createStore(BiFunction<Integer, Integer, Store> supplier, int a, int b) {
        return supplier.apply(a, b);
    }
}
