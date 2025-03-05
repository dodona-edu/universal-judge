#include <exception>
#include "evaluation_result.h"

class Evaluator {
public:
    EvaluationResult* evaluate(const std::exception_ptr &actual) {
        try {
            if(actual) {
                std::rethrow_exception(actual);
            }
        } catch (const std::logic_error& e) {
            EvaluationResult* r = new EvaluationResult(0);
            r->result = true;
            r->readableExpected = e.what();
            r->readableActual = e.what();
            return r;
        }

        EvaluationResult* r = new EvaluationResult(1);
        r->result = false;
        r->readableExpected = "std::logic_error";
        r->readableActual = "something else";
        r->messages.push_back(new Message("Expected std::logic_error, got something else."));
        return r;
    }

    EvaluationResult* runtime(const std::exception_ptr &actual) {
        throw std::out_of_range("hello");
    }
};

Evaluator* evaluator = new Evaluator();
