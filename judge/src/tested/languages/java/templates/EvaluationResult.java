import java.util.ArrayList;
import java.util.List;

public class EvaluationResult {

    public final boolean result;
    public final String readableExpected;
    public final String readableActual;
    public final List<Message> messages;

    private EvaluationResult(boolean result, String readableExpected, String readableActual, List<Message> messages) {
        this.result = result;
        this.readableExpected = readableExpected;
        this.readableActual = readableActual;
        this.messages = messages;
    }

    public static Builder builder(boolean accepted) {
        return new Builder(accepted);
    }

    public static class Message {
        public final String description;
        public final String format;
        public final String permission;

        public Message(String description, String format, String permission) {
            this.description = description;
            this.format = format;
            this.permission = permission;
        }

        public Message(String description, String format) {
            this(description, format, null);
        }

        public Message(String description) {
            this(description, "text");
        }
    }

    public static class Builder {

        private final boolean result;
        private String readableExpected = null;
        private String readableActual = null;
        private final List<Message> messages = new ArrayList<>();

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

        public Builder withMessages(List<Message> messages) {
            this.messages.addAll(messages);
            return this;
        }

        public Builder withMessage(Message message) {
            this.messages.add(message);
            return this;
        }

        public EvaluationResult build() {
            return new EvaluationResult(result, readableExpected, readableActual, messages);
        }
    }
}
