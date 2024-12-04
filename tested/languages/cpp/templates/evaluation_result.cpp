#include <iostream>
#include <vector>
#include <string>

#include "evaluation_result.h"

// Define the Message constructor
Message::Message(const std::string &desc, const std::string &fmt, const std::string &perm)
    : description(desc), format(fmt.empty() ? "text" : fmt), permission(perm) {}

// Define the EvaluationResult constructor
EvaluationResult::EvaluationResult(size_t nrOfMessages) {
    messages.reserve(nrOfMessages);
}

// Define the EvaluationResult destructor
EvaluationResult::~EvaluationResult() {
    for (auto message : messages) {
        delete message;
    }
}

// Function to create an EvaluationResult object
EvaluationResult* create_result(size_t nrOfMessages) {
    return new EvaluationResult(nrOfMessages);
}

// Function to free an EvaluationResult object
void free_result(EvaluationResult *result) {
    delete result;
}

// Function to create a Message object
Message* create_message(const std::string &description, const std::string &format, const std::string &permission) {
    return new Message(description, format, permission);
}

// Function to free a Message object
void free_message(Message *message) {
    delete message;
}








