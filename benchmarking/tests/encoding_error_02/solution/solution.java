import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static double findError(String path, int lengthPreamble) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            double[] numbers = new double[lines.size()];
            for (int i = 0; i < lengthPreamble; i++) {
                double number = Double.parseDouble(lines.get(i).strip());
                numbers[i] = number;
            }
            for (int i = lengthPreamble; i < lines.size(); i++) {
                double sum = Double.parseDouble(lines.get(i).strip());
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

    public static int findWeakness(String path, int lengthPreamble) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            double faulty = findError(path, lengthPreamble);
            double[] numbers = new double[lines.size()];
            for (int i = 0; i < lines.size(); i++) {
                numbers[i] = Double.parseDouble(lines.get(i).strip());
            }
            int i = 0, j = 1;
            while(i < lines.size() - 1) {
                j = i + 1;
                double sum = numbers[i];
                while(j < lines.size()) {
                    sum += numbers[j];
                    if(sum == faulty) {
                        Arrays.sort(numbers, i, j + 1);
                        return (int) (numbers[i] + numbers[j]);
                    }
                    if(sum > faulty) break;
                    j++;
                }
                i++;
            }
            return 0;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
