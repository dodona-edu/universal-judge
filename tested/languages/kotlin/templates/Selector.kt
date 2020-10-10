fun main(args: Array<String>) {
    val name = args[0];
    % for c in contexts:
        if ("${c}".equals(name)) {
            val context = ${c}()
            try {
                context.execute();
            } finally {
                context.close()
            }
        }
    % endfor
}
