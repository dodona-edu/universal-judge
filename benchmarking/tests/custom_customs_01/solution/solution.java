import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static int groupCount(String s) {
        s = s.replaceAll(" ", "");
        Boolean[] seen = new Boolean[26];
        Arrays.fill(seen, false);
        char[] chars = s.toCharArray();
        int count = 0;
        for (int i = 0; i < chars.length; i++) {
            if(!seen[chars[i]-'a']) {
                count++;
                seen[chars[i]-'a'] = true;
            }
        }
        return count;
        /*
        //Simpler but can be up to 4.7 times slower
        Set<Character> set = new HashSet<>(26);
        for (char c: s.toCharArray()) {
            set.add(c);
        }
        return set.size();
        */
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
                    group += line.strip();
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
