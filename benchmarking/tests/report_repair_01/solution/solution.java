import java.util.*;
public class Submission{
    static int repair(List<Integer> list){
        int product = 0;
        for(int i = 0;i<list.size();i++){
            for(int j = i+1;j<list.size();j++){
                if((list.get(i)+list.get(j)) == 2020){
                    product = list.get(i)*list.get(j);
                }
            }
        }
        return product;
    }
}
