using Tested;
using System.Collections;

public class Evaluator {
    public static EvaluationResult Evaluate(Object actual) {
        var correct = "correct" == actual;
        var messages = new List<Message>() {new Tested.Message("Hallo")};
        return new EvaluationResult(correct, "correct", actual != null ? actual.ToString() : "", messages);
    }

    public static EvaluationResult EvaluateSum(Object actual, int sum) {
            var correct = sum == 10;
            var messages = new List<Message>() {new Tested.Message("Hallo")};
            return new EvaluationResult(correct, "correct", actual != null ? actual.ToString() : "", messages);
        }
}
