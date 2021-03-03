import java.util.ArrayList;
import java.util.Collection;
import java.util.Arrays;
import java.util.*;
import java.util.stream.*;

public class Submission{

    public static int repair(List<Integer> test) {
        for (int i=0;i<test.size();i++){
            for (int j=i+1;j<test.size();j++){
                for (int x=j+1;x<test.size();x++){
                    if (test.get(i)+test.get(j)+test.get(x)==2020){
                        return test.get(i)*test.get(j)*test.get(x);
                    }
                }
            }
        }
        return 0;
    }
}
