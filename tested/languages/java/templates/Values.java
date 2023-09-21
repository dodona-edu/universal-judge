import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.lang.reflect.Array;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Minimal RPC language in JSON to send data from the tests to the judge.
 */
public class Values {

    private static String encodeSequence(Iterable<?> objects) {
        var results = new ArrayList<String>();
        for (Object obj : objects) {
            results.add(encode(obj));
        }
        return "[" + String.join(", ", results) + "]";
    }

    public static List<Object> getListFromArray(Object value) {
        if (value instanceof Object[]) {
            return Arrays.asList((Object[]) value);
        }
        int arrayLength = Array.getLength(value);
        List<Object> list = new ArrayList<>();
        for (int i = 0; i < arrayLength; i++) {
            list.add(Array.get(value, i));
        }
        return list;
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
        String diagnostic = null;

        if (value == null) {
            type = "nothing";
            data = "null";
        } else if (value instanceof Boolean) {
            type = "boolean";
            data = value.toString();
        } else if (value.getClass().isArray()) {
            type = "array";
            List<Object> list = new ArrayList<>();
            data = encodeSequence(getListFromArray(value));
        } else if (value instanceof BigInteger) {
            type = "bigint";
            data = value.toString();
        } else if (value instanceof BigDecimal) {
            type = "fixed_precision";
            data = "\"" + escape(value.toString()) + "\"";
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
            Float v = (Float) value;
            type = "single_precision";
            if (v.isNaN()) {
                data = "\"nan\"";
            } else if (!v.isInfinite()) {
                data = v.toString();
            } else if (v < 0.0f) {
                data = "\"-inf\"";
            } else {
                data = "\"inf\"";
            }
        } else if (value instanceof Double) {
            Double v = (Double) value;
            type = "double_precision";
            if (v.isNaN()) {
                data = "\"nan\"";
            } else if (!v.isInfinite()) {
                data = v.toString();
            } else if (v < 0.0) {
                data = "\"-inf\"";
            } else {
                data = "\"inf\"";
            }
        } else if (value instanceof Character) {
            type = "char";
            data = "\"" + escape(value.toString()) + "\"";
        } else if (value instanceof CharSequence) {
            type = "text";
            data = "\"" + escape(value.toString()) + "\"";
        } else if (value instanceof List) {
            type = "list";
            data = encodeSequence((Iterable<?>) value);
        } else if (value instanceof Set) {
            type = "set";
            data = encodeSequence((Iterable<?>) value);
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
            diagnostic = "\"" + escape(((Object) value).getClass().getName()) + "\"";
        }
        return Arrays.asList(type, data, diagnostic);
    }

    private static String encode(Object value) {
        var typeAndData = internalEncode(value);
        return """
               {
                 "data": %s,
                 "type": "%s",
                 "diagnostic": %s
               }
               """.formatted(typeAndData.get(1), typeAndData.get(0), typeAndData.get(2));
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
        String result = """
                {
                  "message": %s,
                  "stacktrace": %s,
                  "type": "%s"
                }
                """.formatted(asJson(msg), asJson(trace), exception.getClass().getSimpleName());
        writer.write(result);
    }

    private static String convertMessage(EvaluationResult.Message message) {
        var description = asJson(message.description);
        var format = asJson(message.format);
        var permission = asJson(message.permission);
        
        return """
                {
                  "description": %s,
                  "format": %s,
                  "permission": %s
                }
                """.formatted(description, format, permission);
    }
    
    private static String asJson(String value) {
        if (value == null) {
            return "null";
        } else {
            return "\"%s\"".formatted(escape(value));
        }
    }

    public static void sendEvaluated(PrintWriter writer, EvaluationResult r) {
        List<String> converted = r.messages.stream().map(Values::convertMessage).collect(Collectors.toList());
        var readableExpected = asJson(r.readableExpected);
        var readableActual = asJson(r.readableActual);
        var dslExpected = asJson(r.dslExpected);
        var dslActual = asJson(r.dslActual);
        var messages = String.join(", ", converted);
        
        String result = """
                {
                  "result": %b,
                  "readable_expected": %s,
                  "readable_actual": %s,
                  "dsl_expected": %s,
                  "dsl_actual": %s,
                  "messages": [%s]
                }
                """.formatted(r.result, readableExpected, readableActual, dslExpected, dslActual, messages);

        writer.print(result);
    }
}
