import java.util.function.Function;

class Submission {

    public static String echo(String value) {
        Function<String, String> id = Function.identity();
        Function<String, String> iden = s -> {
            throw new RuntimeException();
        };
        return id.andThen(iden).apply(value);
    }
}
