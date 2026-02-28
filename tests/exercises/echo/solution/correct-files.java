import java.util.Scanner;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Submission {
    public static void main(String[] args) throws Exception {
        Scanner scanner = new Scanner(System.in);
        String input = scanner.hasNextLine() ? scanner.nextLine() : "";

        File f = new File("input2.txt");
        if (f.exists()) {
            String fileTxt = new String(Files.readAllBytes(Paths.get("input2.txt"))).trim();
            if (!fileTxt.isEmpty()) {
                System.out.println(fileTxt);
            } else {
                System.out.println(input);
            }
        } else {
            System.out.println(input);
        }
    }
}
