import java.io.*;
import java.util.*;

abstract class AbstractCustomEvaluator implements Closeable {

    private final OutputStreamWriter valueWriter;

    public AbstractCustomEvaluator() {
        this.valueWriter = new OutputStreamWriter(System.out);
    }

    @Override
    public void close() throws IOException {
        this.valueWriter.close();
    }

    abstract void evaluate(Object expected, Object actual, List<Object> arguments) throws IOException;

    /**
     * Report the result of an evaluation to the judge. This method should only
     * be called once, otherwise things will break.
     *
     * @param result The result of the evaluation. True if accepted, false otherwise.
     * @param readableExpected Optional string version of the expected value.
     * @param readableActual Optional string version of the acutal value.
     * @param messages Optional list of messages to pass to the student.
     */
    protected void evaluated(boolean result,
                   String readableExpected,
                   String readableActual,
                   Collection<String> messages) throws IOException {
        Values.evaluated(valueWriter, result, readableExpected, readableActual, messages);
    }

    /**
     * Report the result of an evaluation to the judge. This method should only
     * be called once, otherwise things will break.
     *
     * @param result The result of the evaluation. True if accepted, false otherwise.
     * @param readableExpected Optional string version of the expected value.
     * @param readableActual Optional string version of the acutal value.
     */
    protected void evaluated(boolean result,
                             String readableExpected,
                             String readableActual) throws IOException {
        Values.evaluated(valueWriter, result, readableExpected, readableActual, List.of());
    }
}
