import java.io.FileWriter;
import java.io.IOException;

public class Submission {
    public static void generateEven() {
        try (FileWriter writer = new FileWriter("even.txt")) {
            for (int i : new int[]{9, 8, 6, 4, 2}) {
                writer.write(i + "\n");
            }
        } catch (IOException e) {
            System.err.println("Error writing to file: " + e.getMessage());
        }
    }
}
