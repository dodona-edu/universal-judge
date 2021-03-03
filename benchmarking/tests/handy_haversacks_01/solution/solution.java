import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static int holdShinyGold(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            // Parse the rules
            HashMap<String, List<String>> rules = new HashMap<>();
            for (String line: lines) {
                String[] split = line.split(" bags contain ");
                List<String> list = new ArrayList<>();
                for (String bag: split[1].split(", ")) {
                    String newBag = bag.replaceAll("[0-9.]|(bags?)", "").strip();
                    list.add(newBag);
                }
                rules.put(split[0], list);
            }
            //
            HashMap<String, Boolean> holdsGold = new HashMap<>();
            for (String bag: rules.keySet()) {
                holdsGold.put(bag, false);
            }
            boolean changed = true;
            while(changed) {
                changed = false;
                for (String bag: rules.keySet()) {
                    if(holdsGold.get(bag)) continue;
                    for (String containsBag: rules.get(bag)) {
                        if(containsBag.equals("shiny gold") || holdsGold.containsKey(containsBag) && holdsGold.get(containsBag)) {
                            holdsGold.put(bag, true);
                            changed = true;
                        }
                    }
                }
            }
            holdsGold.values().removeIf(aBoolean -> !aBoolean);
            return holdsGold.size();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
