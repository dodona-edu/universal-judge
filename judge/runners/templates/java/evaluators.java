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

class Evaluator {

    private final FileWriter valueWriter;
    private final FileWriter exceptionWriter;

    public Evaluator() throws Exception {
        this.valueWriter = new FileWriter("${value_file}");
        this.exceptionWriter = new FileWriter("${exception_file}");
    }

    void close() throws Exception {
        this.valueWriter.close();
        this.exceptionWriter.close();
    }

    void writeDelimiter(String value) throws Exception {
        valueWriter.write(value);
        exceptionWriter.write(value);
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

    void sendException(Exception exception) throws Exception {
        Values.sendException(exceptionWriter, exception);
    }

    ${main_testcase.exception_code}

    % for additional in additional_testcases:
        % if additional.has_return:
            ${additional.value_code}
        % endif

        ${additional.exception_code}

    % endfor
}