import java.io.PrintWriter

fun main() {
    val writer = PrintWriter(System.out)
    % for statement in statements:
        send(writer, <%include file="statement.mako" args="statement=statement"/>)
        writer.write("\n")
    % endfor
    writer.close()
}
