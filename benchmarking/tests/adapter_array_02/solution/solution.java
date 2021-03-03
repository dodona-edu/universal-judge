import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static long arrangements(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            int[] numbers = new int[lines.size() + 1];
            numbers[0] = 0;
            for (int i = 1; i < numbers.length; i++) {
                numbers[i] = Integer.parseInt(lines.get(i-1).strip());
            }
            Arrays.sort(numbers);
            // Construct a directed graph
            Map<Integer, List<Integer>> neighbours = new TreeMap<>();
            int diff = 3;
            for (int i = 0; i < numbers.length; i++) {
                neighbours.put(numbers[i], new ArrayList<>());
                int j = i + 1;
                while(j < numbers.length && numbers[j] - numbers[i] <= diff) {
                    neighbours.get(numbers[i]).add(numbers[j]);
                    j++;
                }
            }

            // Use Dynamic Programming to solve in O(n*m)
            Map<Integer, Long> counts = new TreeMap<>();
            counts.put(numbers[numbers.length-1], 1L);
            for (int i = numbers.length - 2; i >= 0; i--) {
                long n = 0;
                for (Integer neighbour: neighbours.get(numbers[i])) {
                    n += counts.get(neighbour);
                }
                counts.put(numbers[i], n);
            }
            return counts.get(0);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
