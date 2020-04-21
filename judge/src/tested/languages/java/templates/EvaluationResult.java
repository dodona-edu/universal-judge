import java.util.ArrayList;
import java.util.List;

public class EvaluationResult {

    public final boolean result;
    public final String readableExpected;
    public final String readableActual;
    public final List<String> messages;

    private EvaluationResult(boolean result, String readableExpected, String readableActual, List<String> messages) {
        this.result = result;
        this.readableExpected = readableExpected;
        this.readableActual = readableActual;
        this.messages = messages;
    }

    public static Builder builder(boolean accepted) {
        return new Builder(accepted);
    }

    public static class Builder {

        private final boolean result;
        private String readableExpected = null;
        private String readableActual = null;
        private final List<String> messages = new ArrayList<>();

        private Builder(boolean result) {
            this.result = result;
        }

        public Builder withReadableExpected(String readableExpected) {
            this.readableExpected = readableExpected;
            return this;
        }

        public Builder withReadableActual(String readableActual) {
            this.readableActual = readableActual;
            return this;
        }

        public Builder withMessages(List<String> messages) {
            this.messages.addAll(messages);
            return this;
        }

        public Builder withMessage(String message) {
            this.messages.add(message);
            return this;
        }

        public EvaluationResult build() {
            return new EvaluationResult(result, readableExpected, readableActual, messages);
        }
    }
}
