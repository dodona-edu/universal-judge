class Submission {
    public static int add(int a, int b) {
        return a + b;
    }

    public static String greet(String name) {
        throw new IllegalArgumentException("cannot greet " + name);
    }

    public static void printGreeting(String name) {
        System.out.println("Hello, " + name + "!");
    }
}
