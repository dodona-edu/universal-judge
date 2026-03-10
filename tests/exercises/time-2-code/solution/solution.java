import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Scanner;

public class Submission {
    static String load(String filename) {
        var path = Path.of(filename);
        try {
            return Files.readString(path).trim();
        } catch (IOException e) {
            return "";
        }
    }

    static void save(String user, String filename) {
        var path = Path.of(filename);
        try {
            Files.writeString(path, user + System.lineSeparator(),
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING);
        } catch (IOException e) {
            System.err.println("Error saving file: " + e.getMessage());
        }
    }

    public static void main(String[] args) {
        var user = load("datafile.txt");

        if (user.isEmpty()) {
            System.out.println("Hello, I don't believe we have met.");
            try (var scanner = new Scanner(System.in)) {
                user = scanner.nextLine();
                save(user, "datafile.txt");
                System.out.println("Nice to meet you " + user + ".");
            }
        } else {
            System.out.println("It's good to see you again, " + user + ".");
        }
    }
}
