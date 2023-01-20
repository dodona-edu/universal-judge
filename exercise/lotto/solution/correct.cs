using System;
using System.Collections.Generic;
using System.Linq;

class Submission {
    public static string Loterij(int aantal = 6, int maximum = 42) {
        var r = new Random();
        var result = new HashSet<int>();
        while (result.Count < aantal) {
            result.Add(r.Next(1, maximum + 1));
        }
        var list = result.ToList()
                        .OrderBy(i => i)
                        .Select(i => i.ToString());
        return String.Join(" - ", list);
    }
}
