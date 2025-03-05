#include <iostream>
#include <vector>
#include <string>

#include "evaluation_result.h"
// Define the Message constructor
Message::Message(const std::string &description, const std::string &format, const std::string &permission)
    : description(description), format(format.empty() ? "text" : format), permission(permission) {}

// Define the EvaluationResult constructor
EvaluationResult::EvaluationResult(std::size_t nrOfMessages) {
    messages.reserve(nrOfMessages);
}

// Define the EvaluationResult destructor
EvaluationResult::~EvaluationResult() {
    for (auto message : messages) {
        delete message;
    }
}
