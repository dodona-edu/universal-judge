class Selector {
    public static void main(String[] a) throws Exception {
        var name = a[0];
        % for c in contexts:
            if ("${c}".equals(name)) {
                var context = new ${c}();
                context.execute();
                context.close();
            }
        % endfor
    }
}
