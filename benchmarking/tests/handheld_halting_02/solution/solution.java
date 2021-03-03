import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    static class Pair<F extends String, S extends Integer> {
        public F first;
        public S second;

        public Pair(F f, S s) {
            first = f;
            second = s;
        }
    }

    public static int fix(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            Pair[] instructions = new Pair[lines.size()];
            for (int i = 0; i < lines.size(); i++) {
                String[] split = lines.get(i).split(" ");
                instructions[i] = new Pair<>(split[0], Integer.parseInt(split[1]));
            }
            int i = 0;
            int j = 0;
            int acc = 0;
            boolean[] executed = new boolean[lines.size()];
            while (i != lines.size()) {
                if (instructions[j].first.equals("nop") || instructions[j].first.equals("jmp")) {
                    Arrays.fill(executed, false);
                    i = 0;
                    acc = 0;
                    while (i < lines.size()) {
                        if (executed[i]) {
                            break;
                        }
                        executed[i] = true;

                        if (instructions[i].first.equals("nop") || (instructions[i].first.equals("jmp") && i == j)) {
                            i++;
                        } else if (instructions[i].first.equals("acc")) {
                            acc += instructions[i].second;
                            i++;
                        } else if (instructions[i].first.equals("jmp") || (instructions[i].first.equals("nop") && i == j)) {
                            i += instructions[i].second;
                        }
                    }
                }
                j++;
            }
            return acc;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
