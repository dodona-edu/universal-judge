public void evaluate(Object expected, Object actual) throws Exception {
    boolean acceptable = false;
    evaluated(acceptable, List.of("Hallo", "Actual is " + actual, "Expected is " + expected));
}