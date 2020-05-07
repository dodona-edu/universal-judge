import java.io.*;
import java.util.*;
import java.util.function.*;
public class Context01 implements Closeable {
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;
    public Context01() throws Exception {
        this.valueWriter = new PrintWriter("kEuc7PDIg_values.txt");
        this.exceptionWriter = new PrintWriter("kEuc7PDIg_exceptions.txt");
    }
    private void writeSeparator() throws Exception {
        valueWriter.write("--kEuc7PDIg-- SEP");
        exceptionWriter.write("--kEuc7PDIg-- SEP");
        System.err.print("--kEuc7PDIg-- SEP");
        System.out.print("--kEuc7PDIg-- SEP");
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