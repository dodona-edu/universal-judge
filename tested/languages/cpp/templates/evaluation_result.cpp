#include <iostream>
#include <vector>
#include <string>

#include "evaluation_result.h"

using namespace std;

// Define the Message constructor
Message::Message(const string &description, const string &format, const string &permission)
    : description(description), format(format.empty() ? "text" : format), permission(permission) {}

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
