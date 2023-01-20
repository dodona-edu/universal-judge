using Tested;
using System.Numerics;

namespace Tested
{
  public class Encode
  {
    public static void Main(string[] args)
    {
      StreamWriter stdout = new StreamWriter(Console.OpenStandardOutput());
      stdout.AutoFlush = true;
      Console.SetOut(stdout);

      % for statement in statements:
          Values.WriteValue(stdout, <%include file="statement.mako" args="statement=statement"/>);
          stdout.Write("\n");
      % endfor
    }
  }
}
