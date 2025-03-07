#include <string.h>
#include <stdbool.h>

#include "evaluation_result.h"

class Evaluator {
    public:
    Evaluator() {}

    EvaluationResult evaluate(std::string actual) {
        return EvaluationResult((actual == "correct"), "correct", actual, {
            Message("Hallo")
        });
    }

    EvaluationResult evaluate_sum(std::string actual, int sum) {
        return EvaluationResult((sum == 10), "correct", actual, {
            Message("Hallo")
        });
    }
};

Evaluator evaluator = Evaluator();
