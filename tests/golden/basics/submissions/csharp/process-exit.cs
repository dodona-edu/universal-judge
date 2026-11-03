using System;

class Submission
{
    public static int Add(int a, int b)
    {
        return a + b;
    }

    public static string Greet(string name)
    {
        Environment.Exit(1);
        return null;
    }

    public static void PrintGreeting(string name)
    {
        Console.WriteLine("Hello, " + name + "!");
    }
}
