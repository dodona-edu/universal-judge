using System;

class Submission
{
    public static int Add(int a, int b)
    {
        return a + b;
    }

    public static string Greet(string name)
    {
        throw new ArgumentException("cannot greet " + name);
    }

    public static void PrintGreeting(string name)
    {
        Console.WriteLine("Hello, " + name + "!");
    }
}
