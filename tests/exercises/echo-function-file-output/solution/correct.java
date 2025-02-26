import java.io.FileWriter;
import java.io.IOException;

public class Submission {

    public static void echoFunction(String filename, String stringToWrite) {
        try (FileWriter writer = new FileWriter(filename)) {
            writer.write(stringToWrite);
            writer.write(System.lineSeparator()); // Add a newline
        } catch (IOException e) {
            System.err.println("Error writing to file: " + e.getMessage());
        }
    }
}

