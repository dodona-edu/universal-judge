import java.io.Closeable;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Collection;
import java.util.List;

abstract class AbstractEvaluator implements Closeable {

    protected final OutputStreamWriter writer;

    public AbstractEvaluator() {
        this.writer = new OutputStreamWriter(System.out);
    }

    @Override
    public void close() throws IOException {
        this.writer.close();
    }

    /**
     * Report the result of an evaluation to the judge. This method should only
     * be called once, otherwise things will break.
     *
     * @param result           The result of the evaluation.
     * @param readableExpected Optional string version of the expected value.
     * @param readableActual   Optional string version of the actual value.
     * @param messages         Optional list of messages to pass to the student.
     */
    protected void evaluated(boolean result, String readableExpected,
                             String readableActual,
                             Collection<String> messages) throws IOException {
        Values.evaluated(writer,
                result, readableExpected, readableActual, messages);
    }

    protected void evaluated(boolean result, String readableExpected,
                             String readableActual) throws IOException {
        Values.evaluated(writer,
                result, readableExpected, readableActual, List.of());
    }
}
