#ifndef EVALUATION_RESULT_H
#define EVALUATION_RESULT_H

#include <stdbool.h>

typedef struct Message {
    char* description;
    char* format;
    char* permission;
} Message;

typedef struct EvaluationResult {
    bool result;
    char* readableExpected;
    char* readableActual;
    size_t nrOfMessages;
    Message** messages;
} EvaluationResult;


EvaluationResult* create_result(size_t nrOfMessages);
void free_result(EvaluationResult* result);


Message* create_message(char* description, char* format, char* permission);
void free_message(Message* message);




#endif //EVALUATION_RESULT_H
