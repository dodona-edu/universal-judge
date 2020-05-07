import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Submission {
    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String name = reader.readLine();
        System.out.println(name);
    }
}
