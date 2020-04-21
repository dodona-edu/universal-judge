import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;

/**
 * Minimal RPC language in JSON to send data from the tests to the judge.
 */
public class Values {

    private static String encodeSequence(Iterable<Object> objects) {
        var results = new ArrayList<String>();
        for (Object obj: objects) {
            results.add(encode(obj));
        }
        return "[" + String.join(", ", results) + "]";
    }

    @SuppressWarnings("unchecked")
    private static List<String> internalEncode(Object value) {
        String type;
        String data;

        if (value == null) {
            type = "nothing";
            data = "null";
        } else if (value instanceof Boolean) {
            type = "boolean";
            data = value.toString();
        } else if (value.getClass().isArray()) {
            type = "array";
            data = encodeSequence(Arrays.asList((Object[]) value));
        } else if (value instanceof BigInteger) {
            type = "bigint";
            data = value.toString();
        } else if (value instanceof BigDecimal) {
            type = "fixed_precision";
            data = value.toString();
        } else if (value instanceof Byte) {
            type = "int8";
            data = value.toString();
        } else if (value instanceof Short) {
            type = "int16";
            data = value.toString();
        } else if (value instanceof Integer) {
            type = "int32";
            data = value.toString();
        } else if (value instanceof Long) {
            type = "int64";
            data = value.toString();
        } else if (value instanceof Float) {
            type = "single_precision";
            data = value.toString();
        } else if (value instanceof Double) {
            type = "double_precision";
            data = value.toString();
        } else if (value instanceof Character) {
            type = "char";
            data = "\"" + value.toString() + "\"";
        } else if (value instanceof CharSequence) {
            type = "text";
            data = "\"" + value.toString() + "\"";
        } else if (value instanceof List) {
            type = "list";
            data = encodeSequence((Iterable<Object>) value);
        } else if (value instanceof Set) {
            type = "set";
            data = encodeSequence((Iterable<Object>) value);
        } else if (value instanceof Map) {
            type = "object";
            var elements = new ArrayList<String>();
            for (Map.Entry<Object, Object> entry : ((Map<Object, Object>) value).entrySet()) {
                elements.add("\"" + entry.getKey().toString() + "\": " + encode(entry.getValue()));
            }
            data = "{" + String.join(", ", elements) + "}";
        } else {
            type = "unknown";
            data = value.toString();
        }
        return List.of(type, data);
    }

    private static String encode(Object value) {
        var typeAndData = internalEncode(value);
        return "{ \"data\": " + typeAndData.get(1) + ", \"type\": \"" + typeAndData.get(0) + "\"}";
    }

    public static void send(PrintWriter writer, Object value) {
        writer.print(encode(value));
    }

    public static void sendException(PrintWriter writer, Exception exception) {
        var sw = new StringWriter();
        exception.printStackTrace(new PrintWriter(sw));
        var result = "{ \"message\": \"" + exception.getMessage() + "\", \"type\": \"" + sw.toString() + "\"}";
        writer.write(result);
    }

    public static void evaluated(PrintWriter writer,
                                 boolean result, String expected, String actual, Collection<String> messages) {
        var encodedExpected = internalEncode(expected);
        var encodedActual = internalEncode(actual);
        String builder = "{" +
            "\"result\": " +
            result +
            ", \"readable_expected\": " +
            encodedExpected.get(1) +
            ", \"readable_actual\": " +
            encodedActual.get(1) +
            ", \"messages\": [" +
            String.join(", ", messages) +
            "]}";
        writer.print(builder);
    }
}
