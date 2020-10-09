import java.io.PrintWriter

class Encode {
    companion object {
        @JvmStatic
        fun main(args : Array<String> = emptyArray()) {
            val writer = PrintWriter(System.out)
            % for statement in statements:
                valuesSend(writer, <%include file="statement.mako" args="statement=statement"/>)
                writer.write("\n")
            % endfor
            writer.close()
        }
    }

}
