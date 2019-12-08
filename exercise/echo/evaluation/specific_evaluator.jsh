public void evaluate(Object value) throws Exception {
    boolean acceptable = "test-25".equals(value);
    evaluated(acceptable, "expected", value.toString(), List.of("Hallo"));
}