import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.stream.Collectors;

public class Submission {

    public static String echoFile(String value) throws IOException {
        try (BufferedReader r = new BufferedReader(new FileReader(value))) {
            return r.lines().collect(Collectors.joining("\n"));
        }
    }
}
