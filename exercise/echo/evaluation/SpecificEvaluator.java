import java.util.*;
import java.io.*;

public class SpecificEvaluator extends AbstractSpecificEvaluator {

    @Override
    public void evaluate(Object actual) throws IOException {
        boolean acceptable = "test-25".equals(actual);
        evaluated(
                acceptable,
                "expected",
                actual.toString(),
                List.of("Hallo van uit Java-specifiek!")
        );
    }
}
