using System.Text;

if (args.Length == 0)
{
    Environment.Exit(1);
}

string prefix = args[0];
var builder = new StringBuilder();
foreach (string name in args[1..])
{
    builder.AppendLine(prefix + ": " + name);
}

string? line;
while ((line = Console.ReadLine()) != null)
{
    builder.AppendLine(prefix + ": " + line);
}
Console.Write(builder);
