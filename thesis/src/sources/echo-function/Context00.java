import java.io.*;
import java.util.*;
import java.util.function.*;
public class Context00 implements Closeable {
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;
    public Context00() throws Exception {
        this.valueWriter = new PrintWriter("WQbeY1Lm7_values.txt");
        this.exceptionWriter = new PrintWriter("WQbeY1Lm7_exceptions.txt");
    }
    private void writeSeparator() throws Exception {
        valueWriter.write("--WQbeY1Lm7-- SEP");
        exceptionWriter.write("--WQbeY1Lm7-- SEP");
        System.err.print("--WQbeY1Lm7-- SEP");
        System.out.print("--WQbeY1Lm7-- SEP");
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
        this.writeSeparator();
        try {
            sendValue(Submission.echo("input-1"));
            sendException(null);
        } catch (Exception | AssertionError e) {
            sendException(e);
        }
        this.writeSeparator();
        try {
            sendValue(Submission.echo("input-2"));
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