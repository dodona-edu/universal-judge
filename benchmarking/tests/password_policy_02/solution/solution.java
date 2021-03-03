public class Submission {

    public static boolean isValidPassword (String a, String b) {

        String normal = b.replaceAll("-", " ");
        String[] array = normal.split(" ");
        int posa = Integer.parseInt(array[0]) - 1;
        int posb = Integer.parseInt(array[1]) - 1;
        char ch = b.charAt(b.length() - 1);

        if (a.charAt(posa) == ch && a.charAt(posb) == ch) {
            return false;
        } else if (a.charAt(posa) == ch || a.charAt(posb) == ch) {
            return true;
        }

        return false;
    }
}
