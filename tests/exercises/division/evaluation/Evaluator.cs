using System;
using Tested;

public class Evaluator {
    public static EvaluationResult Evaluate(object? actual) {
        if (actual is DivideByZeroException) {
            return new EvaluationResult(true, actual.ToString(), actual.ToString());
        } else {
            var messages = new List<Message>() { new Message("Expected DivideByZeroException, got something else.") };
            return new EvaluationResult(false, "System.DivideByZeroException", actual == null ? "" : actual.ToString(), messages);
        }
    }

    public static EvaluationResult Runtime(object? actual) {
        throw new ArgumentOutOfRangeException("hello");
    }
}
