using System.Collections.Generic;
using System.Linq;

class Submission
{
    public static int CountPairs(ISet<(int, int)> pairs)
    {
        return pairs.Count;
    }

    public static int SumFirsts(List<(int, int)> pairs)
    {
        return pairs.Sum(pair => pair.Item1);
    }

    public static int SecondOf(Dictionary<string, (int, int)> pairs, string key)
    {
        return pairs[key].Item2;
    }

    public static int First((int, int) pair)
    {
        return pair.Item1;
    }

    public static string Describe((int, string) mixed)
    {
        return mixed.Item1 + mixed.Item2;
    }
}
