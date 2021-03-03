import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static int row(String row) {
        if (row.length() < 10) return -1;
        int begin = 0, einde = 127;
        char[] letters = row.toCharArray();
        for (int i = 0; i < 7; i++){
            if (letters[i] == 'B') {
                begin = begin + ((einde - begin + 1) >> 1);
            } else {
                einde = begin + ((einde - begin - 1) >> 1);
            }
        }
        return begin;
    }

    public static int column(String column) {
        if (column.length() < 10) return -1;
        int begin = 0, einde = 7;
        char[] letters = column.toCharArray();
        for (int i = 0; i < 3; i++){
            if (letters[7 + i] == 'R') {
                begin = begin + ((einde - begin + 1) >> 1);
            } else {
                einde = begin + ((einde - begin - 1) >> 1);
            }
        }
        return begin;
    }

    public static int seatId(String card) {
        if (card.length() < 10) return -1;
        return row(card) * 8 + column(card);
    }

    public static int missingSeatId(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            TreeSet<Integer> ids = new TreeSet<>();
            for (String line: lines) {
                if(!line.isBlank()){
                    ids.add(seatId(line.strip()));
                }
            }
            for(int i = ids.first() + 1; i < ids.last(); i++) {
                if(!ids.contains(i)) return i;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
