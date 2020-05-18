import java.io.*;
import java.util.*;
import java.util.function.*;
public class Context00 implements Closeable {
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;
    public Context00() throws Exception {
        this.valueWriter = new PrintWriter("XweCUlorc_values.txt");
        this.exceptionWriter = new PrintWriter("XweCUlorc_exceptions.txt");
    }
    private void writeSeparator() throws Exception {
        valueWriter.write("--XweCUlorc-- SEP");
        exceptionWriter.write("--XweCUlorc-- SEP");
        System.err.print("--XweCUlorc-- SEP");
        System.out.print("--XweCUlorc-- SEP");
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