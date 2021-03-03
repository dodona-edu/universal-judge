import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;

public class Submission
{
    public static int countTrees(int r, int d, String filename)
    {
        int numTrees = 0;
        try
        {
            Scanner scanner = new Scanner(new File(filename));
            int position = 0;
            int skip = 0;
            while(scanner.hasNextLine())
            {
                String line = scanner.nextLine();
                if(skip > 0)
                {
                    skip--;
                }
                else
                {
                    char symbol = line.charAt(position);
                    if(symbol == '#')
                    {
                        numTrees++;
                    }

                    position = (position + r) % line.length();
                    skip = d - 1;
                }
			}
            scanner.close();
        }
        catch (FileNotFoundException e)
        {
			e.printStackTrace();
        }

        return numTrees;
    }
}
