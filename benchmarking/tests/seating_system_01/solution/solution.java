import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    enum STATE {
        OCCUPIED,
        EMPTY,
        GROUND
    }

    private static HashMap<Character, STATE> map = new HashMap<>() {{
        put('#', STATE.OCCUPIED);
        put('L', STATE.EMPTY);
        put('.', STATE.GROUND);
    }};

    public static long occupiedSeats(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            STATE[][] states = new STATE[lines.size() + 2][lines.get(0).length() + 2];
            for (int i = 0; i < lines.size(); i++) {
                char[] chars = lines.get(i).toCharArray();
                for (int j = 0; j < chars.length; j++) {
                    states[i + 1][j + 1] = map.get(chars[j]);
                }
            }
            int count = 0;
            STATE[][] statesCopy;
            boolean isChanged = true;
            while (isChanged) {
                statesCopy = copyStates(states);
                isChanged = false;
                for (int i = 1; i < states.length - 1; i++) {
                    for (int j = 1; j < states[i].length - 1; j++) {
                        if (statesCopy[i][j] == STATE.EMPTY) {
                            if(countOccupied(statesCopy, i, j) == 0) {
                                states[i][j] = STATE.OCCUPIED;
                                count++;
                                isChanged = true;
                            }
                        } else if(statesCopy[i][j] == STATE.OCCUPIED) {
                            if(countOccupied(statesCopy, i, j) >= 4){
                                states[i][j] = STATE.EMPTY;
                                count--;
                                isChanged = true;
                            }
                        }
                    }
                }
            }
            return count;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }

    private static int countOccupied(STATE[][] states, int i, int j) {
        int count = 0;
        for (int k = -1; k < 2; k++) {
            for (int l = -1; l < 2; l++) {
                if (states[i + k][j + l] == STATE.OCCUPIED && (k != 0 || l != 0)) count++;
            }
        }
        return count;
    }

    private static STATE[][] copyStates(STATE[][] states) {
        STATE[][] statesCopy = new STATE[states.length][];
        for (int i = 0; i < states.length; i++) {
            statesCopy[i] = new STATE[states[i].length];
            System.arraycopy(states[i], 0, statesCopy[i], 0, states[i].length);
        }
        return statesCopy;
    }
}
