class Submission {

    public static void main(String[] args) {
        int som = 0;
        for (String arg : args) {
            try {
                som += Integer.parseInt(arg);
            } catch (NumberFormatException e) {
                System.err.println("som: ongeldige argumenten");
                System.exit(1);
            }
        }
        System.out.println(som);

    }
}