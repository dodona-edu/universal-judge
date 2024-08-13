using System.IO;

class Submission
{
   public static void EchoFunction(string filename, string stringToWrite)
   {
       using (StreamWriter writer = new StreamWriter(filename))
       {
           writer.WriteLine(stringToWrite);
       }
   }
}
