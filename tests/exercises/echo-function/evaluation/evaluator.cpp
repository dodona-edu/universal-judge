#include <string.h>
#include <stdbool.h>

#include "evaluation_result.h"

class Evaluator {
    public:
    Evaluator() {}

    EvaluationResult* evaluate(std::string actual) {
        bool result = (actual == "correct");
        EvaluationResult* r = new EvaluationResult(1);
        r->result = result;
        r->readableExpected = "correct";
        r->readableActual = actual;
        r->messages.push_back(new Message("Hallo"));
        return r;
    }

    EvaluationResult* evaluate_sum(std::string actual, int sum) {
        EvaluationResult* r = new EvaluationResult(1);
        r->result = sum == 10;
        r->readableExpected = "correct";
        r->readableActual = actual;
        r->messages.push_back(new Message("Hallo"));
        return r;
    }
};

Evaluator* evaluator = new Evaluator();
