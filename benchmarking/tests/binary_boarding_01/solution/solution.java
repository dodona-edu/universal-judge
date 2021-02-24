// https://dodona.ugent.be/nl/courses/414/series/4216/activities/923375913

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Submission {

    public static int row(String string){
        int start = 0;
        int end = 127;
        String[] stringArray = string.split("");
        for(String letter : stringArray){
            int middle = (end-start)/2 + start;
            if(letter.equals("F")){
                end = middle;
            } else if(letter.equals("B")){
                start = middle +1;
            }
        }
        return start;
    }

    public static int column(String string){
        int start = 0;
        int end = 7;
        String[] stringArray = string.split("");
        for(int i = 0; i < 3; i++){
            String letter = stringArray[i+7];
            int middle = (end-start)/2 + start;
            if(letter.equals("L")){
                end = middle;
            } else if(letter.equals("R")){
                start = middle +1;
            }
        }
        return start;
    }

    public static int seatId(String string){
        int row = row(string);
        int column = column(string);
        return ((row*8) + column);
    }

    public static int highestSeatId(String string){
        ArrayList<String> seats = new ArrayList<>();

        String fileName = string;
        Scanner inputStream = null;
        try
        {
            inputStream = new Scanner (new File (fileName));
            while (inputStream.hasNextLine ())
            {
                seats.add(inputStream.nextLine());
            }
            inputStream.close ();
        }
        catch (FileNotFoundException e)
        {
            System.exit (0);
        }

        int highest = 0;
        for(String seat : seats){
            int seatId = seatId(seat);

            if(seatId > highest){
                highest = seatId;
            }
        }
        return highest;
    }

    public static void main(String[] args) {
    }
}
