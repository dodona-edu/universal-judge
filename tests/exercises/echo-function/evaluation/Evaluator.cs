using Tested;
using System.Collections;

public class Evaluator {
    public static EvaluationResult Evaluate(Object actual) {
        var correct = "correct" == actual;
        var messages = new List<Message>() {new Tested.Message("Hallo")};
        return new EvaluationResult(correct, "correct", actual != null ? actual.ToString() : "", messages);
    }

    public static EvaluationResult EvaluateValue(IDictionary context) {
        var messages = new List<Message>() {new Tested.Message("Hallo")};
        return new EvaluationResult(context["expected"] == context["actual"], context["expected"].ToString(), context["actual"] != null ? context["actual"].ToString() : "", messages);
    }

    public static EvaluationResult EvaluateValueDsl(IDictionary context) {
            var messages = new List<Message>() {new Tested.Message("Hallo")};
            return new EvaluationResult(context["expected"] == context["actual"], null, null, messages, "{5, 5}", "{4, 4}");
        }
}
