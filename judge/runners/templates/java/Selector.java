class Selector {
    public static void main(String[] a) throws Exception {
        var number = Integer.parseInt(a[0]);
        % for c in contexts:
            if (${loop.index} == number) {
                var context = new ${c}();
                context.execute();
                context.close();
            }
        % endfor
    }
}
