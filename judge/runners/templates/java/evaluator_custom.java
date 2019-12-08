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

public class Evaluator {

    private final FileWriter valueWriter;

    public EvaluatorEval() throws Exception {
        this.valueWriter = new OutputStreamWriter(System.out);
    }

    void evaluated(boolean result, Collection<String> messages) throws Exception {
        Values.evaluated(valueWriter, result, null, null, messages);
    }

    void evaluated(boolean result) throws Exception {
        Values.evaluated(valueWriter, result, null, null, Collections.emptyList());
    }

    ${evaluator_code}

    public static void main(String[] args) throws Exception {
        Evaluatoreval eval = new Evaluatoreval();
        eval.evaluate(\
        <%include file="value.mako" args="value=expected" />
        , \
        <%include file="value.mako" args="value=actual" />
        );
    }
}