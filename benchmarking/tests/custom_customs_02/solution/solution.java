import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static int groupCount(String s) {
        s = s.strip();
        String[] answers = s.split(" ");
        if(answers.length == 1) return answers[0].length();
        int[] freq = new int[26];
        for (String answer : answers) {
            for (char c : answer.toCharArray()) {
                freq[c - 'a']++;
            }
        }
        int count = 0;
        for (int i: freq) {
            if(i == answers.length) count++;
        }
        return count;
    }


    public static int planeCount(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            String group = "";
            int count = 0;
            for (String line: lines) {
                if(line.isBlank()){
                    count += groupCount(group);
                    group = "";
                } else {
                    group += line.strip() + " ";
                }
            }
            if(!lines.get(lines.size()-1).isBlank()) {
                count += groupCount(group);
            }
            return count;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
