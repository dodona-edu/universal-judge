using Tested;
using System.Collections;

public class Evaluator {
    public static EvaluationResult Evaluate(Object actual) {
        var correct = "correct" == actual;
        var messages = new List<Message>() {new Tested.Message("Hallo")};
        return new EvaluationResult(correct, "correct", actual != null ? actual.ToString() : "", messages);
    }

    public static EvaluationResult EvaluateValue(Object expected, Object actual) {
        var messages = new List<Message>() {new Tested.Message("Hallo")};
        return new EvaluationResult(expected == actual, expected.ToString(), actual != null ? actual.ToString() : "", messages);
    }
    
    public static EvaluationResult EvaluateValueDsl(Object expected, Object actual) {
            var messages = new List<Message>() {new Tested.Message("Hallo")};
            return new EvaluationResult(expected == actual, null, null, messages, "{5, 5}", "{4, 4}");
        }
}
