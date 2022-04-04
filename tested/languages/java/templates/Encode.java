import java.util.*;
import java.io.*;
import java.math.BigInteger;
import java.math.BigDecimal;

public class Encode {

    public static void main(String[] args) throws Exception {
        PrintWriter writer = new PrintWriter(System.out);
        % for statement in statements:
            Values.send(writer, <%include file="statement.mako" args="statement=statement"/>);
            writer.write("\n");
        % endfor
        writer.close();
    }

}
