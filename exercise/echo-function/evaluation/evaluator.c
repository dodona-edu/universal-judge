#include <string.h>
#include <stdbool.h>

#include "evaluation_result.h"

EvaluationResult evaluate(char* actual) {
    bool result = !strcmp("correct", actual);
    char* messages[] = {"Hallo"};
    EvaluationResult r = { result, "correct", actual, messages, 1};
    return r;
}