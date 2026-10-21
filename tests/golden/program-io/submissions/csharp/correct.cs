using System;

class Submission
{
    public static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            Environment.Exit(1);
        }

        string prefix = args[0];
        foreach (string name in args[1..])
        {
            Console.WriteLine(prefix + ": " + name);
        }

        string? line;
        while ((line = Console.ReadLine()) != null)
        {
            Console.WriteLine(prefix + ": " + line);
        }
    }
}
