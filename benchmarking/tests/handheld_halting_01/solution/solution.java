import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static int inspect(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            boolean[] executed = new boolean[lines.size()];
            int acc = 0;
            int i = 0;
            while(i < lines.size()) {
                if(executed[i]){
                    break;
                }
                executed[i] = true;
                String[] split = lines.get(i).split(" ");
                if(split[0].equals("nop")){
                    i++;
                } else if(split[0].equals("acc")) {
                    acc += Integer.parseInt(split[1]);
                    i++;
                } else if(split[0].equals("jmp")) {
                    i += Integer.parseInt(split[1]);
                }
            }
            return acc;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
