#include <exception>
#include "evaluation_result.h"

class Evaluator {
public:
    static EvaluationResult evaluate(const std::exception_ptr &actual) {
        try {
            if(actual) {
                std::rethrow_exception(actual);
            }
        } catch (const std::logic_error& e) {
            return EvaluationResult(true, e.what(), e.what());
        } catch (...) {
            // do nothing
        }

        return EvaluationResult(false, "std::logic_error", "something else", {
            Message("Expected std::logic_error, got something else.")
        });
    }

    static EvaluationResult runtime(const std::exception_ptr &actual) {
        throw std::out_of_range("hello");
    }
};
