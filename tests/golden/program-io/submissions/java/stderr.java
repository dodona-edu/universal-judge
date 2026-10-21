import java.util.Scanner;

public class Submission {
    public static void main(String[] args) {
        System.err.println("debug: " + args.length + " arguments");

        if (args.length == 0) {
            System.exit(1);
        }

        String prefix = args[0];
        for (int i = 1; i < args.length; i++) {
            System.out.println(prefix + ": " + args[i]);
        }

        Scanner scanner = new Scanner(System.in);
        while (scanner.hasNextLine()) {
            System.out.println(prefix + ": " + scanner.nextLine());
        }
    }
}
