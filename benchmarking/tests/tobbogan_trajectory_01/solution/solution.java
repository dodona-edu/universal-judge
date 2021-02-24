// https://dodona.ugent.be/nl/courses/414/series/4179/activities/296899168

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Submission {

    // day 1 - part one
    public static int repair1(List<Integer> list) {
        int number1 = 0;
        int number2 = 0;
        int product = 0;
        ArrayList<Integer> array = new ArrayList<>();
        for (int m = 0; m < list.size(); m++) {
            array.add(list.get(m));
        }
        for (int i = 0; i < array.size(); i++) {
            ArrayList<Integer> arrayNew = new ArrayList<>();
            number1 = array.get(i);
            for (int j = 0; j < i; j++) {
                arrayNew.add(array.get(j));
            }
            for (int j = i + 1; j < array.size(); j++) {
                arrayNew.add(array.get(j));
            }
            for (int k = 0; k < arrayNew.size(); k++) {
                number2 = array.get(k);
                if (number1 + number2 == 2020) {
                    product = number1 * number2;
                }
            }
        }
        return product;
    }

    // day 1 - part two
    public static int repair2(List<Integer> list) {
        int number1;
        int number2;
        int number3;
        int product = 0;
        ArrayList<Integer> array = new ArrayList<>();
        for (int m = 0; m < list.size(); m++) {
            array.add(list.get(m));
        }

            for (int i = 0; i < array.size(); i++) {
                ArrayList<Integer> arrayToWork = new ArrayList<>();
                for (int m = 0; m < list.size(); m++) {
                    arrayToWork.add(list.get(m));
                }

                ArrayList<Integer> arrayNew = new ArrayList<>();
                number1 = array.get(i);
                arrayToWork.remove(i);
                arrayNew = arrayToWork;
                for (int j = 0; j < arrayNew.size(); j++) {
                    ArrayList<Integer> arrayNew2 = new ArrayList<>();
                    number2 = arrayNew.get(j);
                    arrayNew.remove(j);
                    arrayNew2 = arrayNew;
                    for (int k = 0; k < arrayNew2.size(); k++) {
                        number3 = arrayNew2.get(k);
                        if (number1 + number2 + number3 == 2020) {
                            product = number1 * number2 * number3;
                        }
                    }

                }
            }
        return product;
    }

    // day 2 - part one
    public static boolean isValidPassword1(String wachtwoord, String beleid){
        boolean result = false;

        String[] parts = beleid.split("-");
        String[] parts2 = parts[1].split(" ");

        int lowerB = Integer.parseInt(parts[0]);
        int upperB = Integer.parseInt(parts2[0]);
        String character = parts2[1];

        int amountOfCharacterGiven = 0;
        String[] password = wachtwoord.split("");

        for(int i = 0; i < password.length; i++){
            if(password[i].equals(character)){
                amountOfCharacterGiven++;
            }
        }
        if((amountOfCharacterGiven >= lowerB) && (amountOfCharacterGiven <= upperB)){
            result = true;
        }

        return result;
    }

    // day 2 - part two
    public static boolean isValidPassword(String wachtwoord, String beleid){
        boolean result = false;

        String[] parts = beleid.split("-");
        String[] parts2 = parts[1].split(" ");

        int lowerB = Integer.parseInt(parts[0]);
        int upperB = Integer.parseInt(parts2[0]);
        String character = parts2[1];

        int amountOfCharacterGiven = 0;
        String[] password = wachtwoord.split("");
        if((password[lowerB-1].equals(character) && !password[upperB-1].equals(character)) || (!password[lowerB-1].equals(character) && password[upperB-1].equals(character))){
            result = true;
        }

        return result;
    }

    // day 3 - part one
    public static int countTrees(String fileName) throws FileNotFoundException {
        Scanner inputStream = null;
        try
        {
            File file = new File(fileName);
            inputStream = new Scanner (file);
        }
        catch (FileNotFoundException e)
        {
            System.out.println ("Error opening the file " + fileName);
            System.exit (0);
        }
        int amountOfLines = 1;
        int amountOnLine = 0;
        ArrayList<String> txtFileArray = new ArrayList<>();
        txtFileArray.add(inputStream.nextLine());
        while (inputStream.hasNextLine ())
        {
            String line = inputStream.nextLine ();
            txtFileArray.add(line);
            amountOnLine = line.length();

            amountOfLines++;
        }
        inputStream.close ();

        int xPosition = 0;
        int amountOfTrees = 0;

        for(String string : txtFileArray){
            int index = xPosition % amountOnLine;
            if(string.split("")[index].equals("#"))
                amountOfTrees++;
            xPosition += 3;
        }
        return amountOfTrees;
    }

    // day 3 - part two
    public static int countTrees1(int right, int down, String fileName) throws FileNotFoundException {
        Scanner inputStream = null;
        try
        {
            File file = new File(fileName);
            inputStream = new Scanner (file);
        }
        catch (FileNotFoundException e)
        {
            System.out.println ("Error opening the file " + fileName);
            System.exit (0);
        }
        int amountOfLines = 1;
        int amountOnLine = 1000;
        ArrayList<String> txtFileArray = new ArrayList<>();
        txtFileArray.add(inputStream.nextLine());
        while (inputStream.hasNextLine ())
        {
            String line = inputStream.nextLine ();
            txtFileArray.add(line);
            amountOnLine = line.length();

            amountOfLines++;
        }
        inputStream.close ();

        ArrayList<String> txtFileArray2 = new ArrayList<>();

        int indexCurrentLine = 0;
        for(int k = 0; k < amountOfLines; k +=down){
            txtFileArray2.add(txtFileArray.get(indexCurrentLine));
            indexCurrentLine += down;
        }

        int xPosition = 0;
        int amountOfTrees = 0;

        for(String string : txtFileArray){
            int index = xPosition % amountOnLine;
            if(string.split("")[index].equals("#"))
                amountOfTrees++;
            xPosition += right;
        }
        return amountOfTrees;
    }

    public static void main(String[] args) {
        try {
            System.out.println(countTrees1(10,3,"src/helling2"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
