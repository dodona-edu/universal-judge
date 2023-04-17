int som = 0;
foreach (string arg in args)
{
  try
  {
    som += Int32.Parse(arg);
  }
  catch (FormatException)
  {
    Console.Error.WriteLine("som: ongeldige argumenten");
    Environment.Exit(1);
  }
}
Console.WriteLine(som);
