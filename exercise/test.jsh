import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;


BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
String name = reader.readLine();
System.out.println(name);


public String echo(String a, int b) {
    System.out.println(a + "-" + b);
    return a + "-" + b;
}

public void echo2(String a, int b) {
    System.out.println(a + "-" + b);
    var c = a + "-" + b;
}

