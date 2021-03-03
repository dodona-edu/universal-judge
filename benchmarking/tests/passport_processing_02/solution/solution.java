import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Submission {

    public static boolean isValidField(String key, String value) {
        switch (key){
            case "byr": {
                if(value.length() != 4) return false;
                int v = Integer.parseInt(value);
                return 1920 <= v && v <= 2002;
            }
            case "iyr": {
                if(value.length() != 4) return false;
                int v = Integer.parseInt(value);
                return 2010 <= v && v <= 2020;
            }
            case "eyr": {
                if(value.length() != 4) return false;
                int v = Integer.parseInt(value);
                return 2020 <= v && v <= 2030;
            }
            case "hgt": {
                if(value.contains("cm")) {
                    int v = Integer.parseInt(value.split("cm")[0]);
                    return 150 <= v && v <= 193;
                } else if(value.contains("in")) {
                    int v = Integer.parseInt(value.split("in")[0]);
                    return 59 <= v && v <= 76;
                }
                return false;
            }
            case "hcl": {
                return value.matches("#([0-9a-f]){6}");
            }
            case "ecl": {
                return Set.of("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value);
            }
            case "pid": {
                return value.matches("[0-9]{9}");
            }
            default:
                return true;
        }
    }


    public static boolean isValidPassport(String passport) {
        String[] pairs = passport.split(" ");
        Set<String> fields = new HashSet<String>();
        for (int i = 0; i < pairs.length; i++) {
            String[] pair = pairs[i].split(":");
            String key = pair[0];
            String value = pair[1];
            if(!isValidField(key, value)) return false;
            fields.add(key);
        }
        return fields.containsAll(Set.of("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"));
    }


    public static int countValidPassports(String passports) {
        Path file = Path.of(passports);
        try {
            List<String> lines = Files.readAllLines(file);
            int count = 0;
            String passport = "";
            for (int i = 0; i < lines.size(); i++) {
                if(lines.get(i).isBlank()) {
                    if(isValidPassport(passport.strip())) count++;
                    passport = "";
                } else {
                    passport += " " + lines.get(i);
                }
            }
            if(!lines.get(lines.size()-1).isBlank()) {
                if(isValidPassport(passport.strip())) count++;
            }
            return count;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}
