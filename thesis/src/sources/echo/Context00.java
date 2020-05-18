import java.io.*;
import java.util.*;
import java.util.function.*;
public class Context00 implements Closeable {
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;
    public Context00() throws Exception {
        this.valueWriter = new PrintWriter("MOJewuyk5_values.txt");
        this.exceptionWriter = new PrintWriter("MOJewuyk5_exceptions.txt");
    }
    private void writeSeparator() throws Exception {
        valueWriter.write("--MOJewuyk5-- SEP");
        exceptionWriter.write("--MOJewuyk5-- SEP");
        System.err.print("--MOJewuyk5-- SEP");
        System.out.print("--MOJewuyk5-- SEP");
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
        try(Context00 context = new Context00()) {
            context.execute();
        }
    }
}