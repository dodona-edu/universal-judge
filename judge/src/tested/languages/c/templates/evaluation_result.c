//
// Created by strij on 1/05/2020.
//

#include <stdint.h>
#include <stdlib.h>

#include "evaluation_result.h"

EvaluationResult *create_result(size_t nrOfMessages) {
    EvaluationResult* result = malloc(sizeof(EvaluationResult));
    result->messages = malloc(nrOfMessages*sizeof(Message*));
    result->nrOfMessages = nrOfMessages;
    return result;
}

void free_result(EvaluationResult *result) {
    for (size_t i = 0; i < result->nrOfMessages; i++) {
        free(result->messages[i]);
    }
    free(result);
}

Message *create_message(char *description, char *format, char *permission) {
    Message* message = malloc(sizeof(Message));
    message->description = description;
    if (format == NULL) {
        message->format = "text";
    } else {
        message->format = format;
    }
    message->permission = permission;
    return message;
}

void free_message(Message *message) {
    free(message);
}







