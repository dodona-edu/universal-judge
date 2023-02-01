using Tested;
using System.Collections;

public class Evaluator {
    public static EvaluationResult Evaluate(Object actual) {
        var correct = "correct" == actual;
        var messages = new List<Message>() {new Tested.Message("Hallo")};
        return new EvaluationResult(correct, "correct", actual != null ? actual.ToString() : "", messages);
    }

    public static EvaluationResult EvaluateValue(Object expected, Object actual, IList arguments) {
        var messages = new List<Message>() {new Tested.Message("Hallo")};
        return new EvaluationResult(expected == actual, expected.ToString(), actual != null ? actual.ToString() : "", messages);
    }
}
