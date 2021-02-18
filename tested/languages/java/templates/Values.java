import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Minimal RPC language in JSON to send data from the tests to the judge.
 */
public class Values {

    private static String encodeSequence(Iterable<Object> objects) {
        var results = new ArrayList<String>();
        for (Object obj : objects) {
            results.add(encode(obj));
        }
        return "[" + String.join(", ", results) + "]";
    }
        
    private static String escape(String string) {
        return string.replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                        .replace("\b", "\\b")
                        .replace("\f", "\\f")
                        .replace("\n", "\\n")
                        .replace("\r", "\\r")
                        .replace("\t", "\\t");
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
            type = "character";
            data = "\"" + escape(value.toString()) + "\"";
        } else if (value instanceof CharSequence) {
            type = "text";
            data = "\"" + escape(value.toString()) + "\"";
        } else if (value instanceof List) {
            type = "list";
            data = encodeSequence((Iterable<Object>) value);
        } else if (value instanceof Set) {
            type = "set";
            data = encodeSequence((Iterable<Object>) value);
        } else if (value instanceof Map) {
            type = "map";
            var elements = new ArrayList<String>();
            for (Map.Entry<?, ?> entry : ((Map<?, ?>) value).entrySet()) {
                elements.add("{ \"key\":" + encode(entry.getKey()) + ",\"value\": " + encode(entry.getValue()) + "}");
            }
            data = "[" + String.join(", ", elements) + "]";
        } else {
            type = "unknown";
            data = "\"" + escape(value.toString()) + "\"";
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

    public static void sendException(PrintWriter writer, Throwable exception) {
        if (exception == null) {
            return;
        }
        var sw = new StringWriter();
        exception.printStackTrace(new PrintWriter(sw));
        var trace = sw.toString();
        var msg = exception.getMessage();
        var result = "{ \"message\": \"" + escape(msg == null ? "" : msg) +
                     "\", \"stacktrace\": \"" + escape(trace) + "\"}";
        writer.write(result);
    }

    private static String convertMessage(EvaluationResult.Message message) {
        String result = "{" +
            "\"description\": \"" + message.description + "\"," +
            "\"format\": \"" + message.format + "\"";
        if (message.permission instanceof String) {
            result += ", \"permission\": \"" + message.permission + "\"";
        }
        return result + "}";
    }

    public static void evaluated(PrintWriter writer,
                                 boolean result, String expected, String actual, Collection<EvaluationResult.Message> messages) {
        List<String> converted = messages.stream().map(Values::convertMessage).collect(Collectors.toList());
        String builder = "{" +
            "\"result\": " +
            result +
            ", \"readable_expected\": \"" + expected + "\"" +
            ", \"readable_actual\": \"" + actual + "\"" +
            ", \"messages\": [" +
            String.join(", ", converted) +
            "]}";
        writer.print(builder);
    }

    public static void sendEvaluated(PrintWriter writer, EvaluationResult r) {
        evaluated(writer, r.result, r.readableExpected, r.readableActual, r.messages);
    }
}
