string user = Load("datafile.txt");

if (string.IsNullOrEmpty(user))
{
    Console.WriteLine("Hello, I don't believe we have met.");
    user = Console.ReadLine() ?? "";
    Save(user, "datafile.txt");
    Console.WriteLine($"Nice to meet you {user}.");
}
else
{
    Console.WriteLine($"It's good to see you again, {user}.");
}

// -------------------------
// Subprograms (Local Functions)
// -------------------------

string Load(string filename)
{
    try
    {
        return File.ReadAllText(filename).Trim();
    }
    catch (FileNotFoundException)
    {
        return "";
    }
}

void Save(string nameToSave, string filename)
{
    // Using Environment.NewLine is the modern C# equivalent to "\n"
    File.WriteAllText(filename, nameToSave + Environment.NewLine);
}
