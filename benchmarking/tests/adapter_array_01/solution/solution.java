import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static int differences(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            int[] numbers = new int[lines.size()];
            for (int i = 0; i < lines.size(); i++) {
                numbers[i] = Integer.parseInt(lines.get(i).strip());
            }
            Arrays.sort(numbers);
            int n1 = 0;
            int n3 = 1;
            if (numbers[0] == 1) n1++;
            else if (numbers[0] == 3) n3++;
            for (int i = 1; i < numbers.length; i++) {
                int diff = numbers[i] - numbers[i - 1];
                if (diff == 1) n1++;
                else if (diff == 3) n3++;
            }
            return n1 * n3;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
