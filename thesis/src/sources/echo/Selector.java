class Selector {
    public static void main(String[] a) throws Exception {
        var name = a[0];
        if ("Context00".equals(name)) {
            Context00.main(new String[]{});
        }
        if ("Context01".equals(name)) {
            Context01.main(new String[]{});
        }
    }
}