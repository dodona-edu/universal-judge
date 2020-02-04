import java.util.*;
import java.io.*;

public class Evaluator extends AbstractCustomEvaluator {

    @Override
    public void evaluate(Object expected, Object actual, List<Object> arguments) throws IOException {
        evaluated(
                false,
                expected.toString(),
                actual.toString(),
                List.of("Hallo van uit Java!")
        );
    }
}
