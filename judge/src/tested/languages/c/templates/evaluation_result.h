#ifndef EVALUATION_RESULT_H
#define EVALUATION_RESULT_H

typedef struct EvaluationResult {
    bool result;
    char* readableExpected;
    char* readableActual;
    char** messages;
    size_t nrOfMessages;
} EvaluationResult;


#endif //EVALUATION_RESULT_H
