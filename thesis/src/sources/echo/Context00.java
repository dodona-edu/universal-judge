import java.io.Closeable;
import java.io.IOException;
import java.io.PrintWriter;
public class Context00 implements Closeable {
    private final PrintWriter valueWriter;
    private final PrintWriter exceptionWriter;
    public Context00() throws Exception {
        this.valueWriter = new PrintWriter("r3Nf2Aq6C_values.txt");
        this.exceptionWriter = new PrintWriter("r3Nf2Aq6C_exceptions.txt");
    }
    private void writeSeparator() throws Exception {
        valueWriter.write("--r3Nf2Aq6C-- SEP");
        exceptionWriter.write("--r3Nf2Aq6C-- SEP");
        System.err.print("--r3Nf2Aq6C-- SEP");
        System.out.print("--r3Nf2Aq6C-- SEP");
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