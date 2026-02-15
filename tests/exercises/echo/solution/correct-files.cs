using System;
using System.IO;

string input = Console.ReadLine();
if (File.Exists("input2.txt")) {
    string fileTxt = File.ReadAllText("input2.txt").Trim();
    if (!string.IsNullOrEmpty(fileTxt)) {
        Console.WriteLine(fileTxt);
    } else {
        Console.WriteLine(input);
    }
} else {
    Console.WriteLine(input);
}
