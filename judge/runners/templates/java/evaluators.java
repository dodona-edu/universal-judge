## This imports are defined by the "common" start-up scripts of JShell.
import java.io.*;
import java.math.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;
import java.util.prefs.*;
import java.util.regex.*;
import java.util.stream.*;

class Evaluator${context_id} {

    private final FileWriter valueWriter;

    public Evaluator${context_id}() throws Exception {
        this.valueWriter = new FileWriter("${value_file}");
    }

    void close() throws Exception {
        this.valueWriter.close();
    }

    void valueWrite(String value) throws Exception {
        valueWriter.write(value);
    }

    void evaluated(boolean result, String expected, String actual, Collection<String> messages) throws Exception {
        Values.evaluated(valueWriter, result, expected, actual, messages);
    }

    void evaluated(boolean result, String expected, String actual) throws Exception {
        Values.evaluated(valueWriter, result, expected, actual, Collections.emptyList());
    }

    void send(Object value) throws Exception {
        Values.send(valueWriter, value);
    }

    % for additional in additionals:
        % if additional.has_return:
            ${additional.value_code}
        % endif

    % endfor
}