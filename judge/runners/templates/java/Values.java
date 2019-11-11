import java.io.IOException;
import java.io.OutputStreamWriter;

/**
 * Minimal RPC language in JSON to send data from the tests to the judge.
 */
class Values {

    public static void send(OutputStreamWriter writer, Object value) throws IOException {
        if (value == null) {
            return;
        }
        String data = value.toString();
        String type;
        if (value instanceof CharSequence) {
            type = "text";
            data = "\"" + value + "\"";
        } else if (value instanceof Integer) {
            type = "integer";
        } else if (value instanceof Double || value instanceof Float) {
            type = "rational";
        } else if (value instanceof Boolean) {
            type = "boolean";
        } else {
            type = "unknown";
        }

        String result = "{\"data\": %s, \"type\": \"%s\"}";
        String filled = String.format(result, data, type);
        writer.write(filled);
    }

    public static void evaluated(OutputStreamWriter writer, boolean value, String string) throws IOException {
        String result = "{\"string\": \"%s\", \"type\": \"evaluated\", \"accepted\": %b}";
        String filled = String.format(result, string, value);
        writer.write(filled);
    }
}
