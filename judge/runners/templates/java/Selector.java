class Selector {
    public static void main(String[] a) throws Exception {
        var name = a[0];
        % for c in contexts:
            if ("${c}".equals(name)) {
                ${c}.main(new String[]{});
            }
        % endfor
    }
}
