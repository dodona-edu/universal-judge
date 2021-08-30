import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;

class Submission {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        // Getal inlezen
        String getal = reader.readLine();
        Map<Character, Integer> toevoegen = Map.of('0', 1, '4', 1, '6', 1, '8', 2, '9', 1);

        int cirkels = 0;
        
        // Aantal cirkels in de cijfers van het getal tellen
        for (int i = 0; i < getal.length(); i++) {
            cirkels += toevoegen.getOrDefault(getal.charAt(i), 0);
        }

        // Aantal cirkels uitschrijven
        System.out.println(cirkels);
    }
}