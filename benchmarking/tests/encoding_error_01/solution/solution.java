import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static int findError(String path, int lengthPreamble) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            int[] numbers = new int[lines.size()];
            for (int i = 0; i < lengthPreamble; i++) {
                int number = Integer.parseInt(lines.get(i).strip());
                numbers[i] = number;
            }
            for (int i = lengthPreamble; i < lines.size(); i++) {
                int sum = Integer.parseInt(lines.get(i).strip());
                for (int j = i - lengthPreamble; j < i; j++) {
                    for (int k = j + 1; k < i; k++) {
                        if (sum == numbers[j] + numbers[k]) {
                            numbers[i] = sum;
                            break;
                        }
                    }
                }
                if(numbers[i] != sum) return sum;
            }
            return 0;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
