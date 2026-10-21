using System;

class Submission
{
    public static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            Environment.Exit(1);
        }

        Console.WriteLine(args[0]);
        throw new InvalidOperationException("boom");
    }
}
