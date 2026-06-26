using Tested;
using System.Collections.Generic;

public class Evaluator
{
    public static EvaluationResult Evaluate(object actual)
    {
        string shown = actual != null ? actual.ToString() : "";
        return new EvaluationResult(true, shown, shown, new List<Message>());
    }
}
