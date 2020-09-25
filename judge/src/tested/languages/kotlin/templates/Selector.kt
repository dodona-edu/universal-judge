fun main(args: Array<String>) {
    val name = args[0];
    % for c in contexts:
        if ("${c}".equals(name)) {
            ${c}.main();
        }
    % endfor
}
