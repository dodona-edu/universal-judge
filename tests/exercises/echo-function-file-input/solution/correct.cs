using System;
using System.IO;

class Submission
{
    public static string EchoFile(string value)
    {
        return System.IO.File.ReadAllText(value);
    }
}
