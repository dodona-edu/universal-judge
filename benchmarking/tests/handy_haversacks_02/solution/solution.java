import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static int shinyGoldSize(String path) {
        Path file = Path.of(path);
        try {
            List<String> lines = Files.readAllLines(file);
            // Parse the rules
            HashMap<String, Map<String, Integer>> rules = new HashMap<>();
            HashMap<String, Integer> sizes = new HashMap<>();
            for (String line: lines) {
                String[] split = line.split(" bags contain ");
                Map<String, Integer> map = new HashMap<>();
                for (String bag: split[1].split(", ")) {
                    String newBag = bag.replaceAll("[0-9.]|(bags?)", "").strip();
                    if(!newBag.equals("no other")) {
                        int n = Integer.parseInt(bag.replaceAll("[^0-9]", "").strip());
                        map.put(newBag, n);
                    } else {
                        sizes.put(split[0], 0);
                    }
                }
                rules.put(split[0], map);
            }
            while(sizes.size() != rules.size()) {
                for (String bag: rules.keySet()) {
                    if(!sizes.containsKey(bag) && sizes.keySet().containsAll(rules.get(bag).keySet())) {
                        int n = 0;
                        for (String containsBag: rules.get(bag).keySet()) {
                            n += rules.get(bag).get(containsBag) * (sizes.get(containsBag) + 1);
                        }
                        sizes.put(bag, n);
                    }
                }
            }
            return sizes.get("shiny gold");
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
