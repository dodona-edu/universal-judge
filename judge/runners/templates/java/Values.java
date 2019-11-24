import com.github.cliftonlabs.json_simple.JsonObject;
import com.github.cliftonlabs.json_simple.Jsonable;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.*;

/**
 * Minimal RPC language in JSON to send data from the tests to the judge.
 */
class Values {

    @SuppressWarnings("unchecked")
    private static Jsonable encode(Object value) {
        String type;
        Object data;

        if (value == null) {
            type = "nothing";
            data = null;
        } else if (value instanceof CharSequence) {
            type = "text";
            data = value.toString();
        } else if (value instanceof Byte || value instanceof Short || value instanceof Integer || value instanceof Long) {
            type = "integer";
            data = value;
        } else if (value instanceof Float || value instanceof Double) {
            type = "rational";
            data = value;
        } else if (value instanceof List) {
            type = "list";
            List<Jsonable> list = new ArrayList<>();
            for (Object object: (List) value) {
                list.add(encode(object));
            }
            data = list;
        } else if (value instanceof Object[]) {
            type = "list";
            List<Jsonable> list = new ArrayList<>();
            for (Object object: (Object[]) value) {
                list.add(encode(object));
            }
            data = list;
        } else if (value instanceof Set) {
            type = "set";
            List<Jsonable> list = new ArrayList<>();
            for (Object object: (Set) value) {
                list.add(encode(object));
            }
            data = list;
        } else if (value instanceof Map) {
            type = "object";
            Map<String, Jsonable> map = new HashMap<>();
            //noinspection unchecked
            for (Map.Entry<Object, Object> entry : ((Map<Object, Object>) value).entrySet()) {
                map.put(entry.getKey().toString(), encode(entry.getValue()));
            }
            data = map;
        } else {
            type = "unknown";
            data = value.toString();
        }

        JsonObject object = new JsonObject();
        object.put("data", data);
        object.put("type", type);
        return object;
    }

    public static void send(OutputStreamWriter writer, Object value) throws IOException {
        Jsonable object = encode(value);
        object.toJson(writer);
    }

    public static void sendException(OutputStreamWriter writer, Exception exception) throws IOException {
        var sw = new StringWriter();
        exception.printStackTrace(new PrintWriter(sw));
        JsonObject object = new JsonObject();
        object.put("message", exception.getMessage());
        object.put("stacktrace", sw.toString());
        object.toJson(writer);
    }

    public static void evaluated(OutputStreamWriter writer,
                                 boolean result, String expected, String actual, Collection<String> messages) throws IOException {
        JsonObject object = new JsonObject();
        object.put("result", result);
        object.put("readable_expected", expected);
        object.put("readable_actual", actual);
        object.put("messages", messages);
        object.toJson(writer);
    }
}
