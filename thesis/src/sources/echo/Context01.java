import java.io.*;
import java.util.*;
import java.util.function.*;
public class Context01 implements Closeable {
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;
    public Context01() throws Exception {
        this.valueWriter = new PrintWriter("J76WUexjc_values.txt");
        this.exceptionWriter = new PrintWriter("J76WUexjc_exceptions.txt");
    }
    private void writeSeparator() throws Exception {
        valueWriter.write("--J76WUexjc-- SEP");
        exceptionWriter.write("--J76WUexjc-- SEP");
        System.err.print("--J76WUexjc-- SEP");
        System.out.print("--J76WUexjc-- SEP");
        valueWriter.flush();
        exceptionWriter.flush();
        System.err.flush();
        System.out.flush();
    }
    private void sendValue(Object value) throws Exception {
        Values.send(valueWriter, value);
    }
    private void sendException(Throwable exception) throws Exception {
        Values.sendException(exceptionWriter, exception);
    }
    private void sendSpecificValue(EvaluationResult value) {
        Values.sendEvaluated(valueWriter, value);
    }
    private void sendSpecificException(EvaluationResult exception) {
        Values.sendEvaluated(exceptionWriter, exception);
    }
    void execute() throws Exception {
        this.writeSeparator();
        try {
            Submission.main(new String[]{});
            sendException(null);
        } catch (Exception | AssertionError e) {
            sendException(e);
        }
    }
    @Override
    public void close() throws IOException {
        this.valueWriter.close();
        this.exceptionWriter.close();
    }
    public static void main(String[] a) throws Exception {
        try(Context01 context = new Context01()) {
            context.execute();
        }
    }
}