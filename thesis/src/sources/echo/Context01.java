import java.io.*;
import java.util.*;
import java.util.function.*;
public class Context01 implements Closeable {
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;
    public Context01() throws Exception {
        this.valueWriter = new PrintWriter("izH9w8XbJ_values.txt");
        this.exceptionWriter = new PrintWriter("izH9w8XbJ_exceptions.txt");
    }
    private void writeSeparator() throws Exception {
        valueWriter.write("--izH9w8XbJ-- SEP");
        exceptionWriter.write("--izH9w8XbJ-- SEP");
        System.err.print("--izH9w8XbJ-- SEP");
        System.out.print("--izH9w8XbJ-- SEP");
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
    private void sendSpecificValue(EvaluationResult r) {
        Values.sendEvaluated(valueWriter, r);
    }
    private void sendSpecificException(EvaluationResult r) {
        Values.sendEvaluated(exceptionWriter, r);
    }
    void execute() throws Exception {
        this.writeSeparator();
        try {
            Submission.main(new String[]{            });
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