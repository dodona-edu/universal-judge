#include <iostream>
#include <vector>
#include <string>

#include "evaluation_result.h"
// Define the Message constructor
Message::Message(const std::string &description, const std::string &format, const std::string &permission)
    : description(description), format(format.empty() ? "text" : format), permission(permission) {}

// Define the EvaluationResult constructor
EvaluationResult::EvaluationResult(const bool &result, const std::string &readableExpected, const std::string &readableActual, const std::vector<Message> &messages)
    : result(result), readableExpected(readableExpected), readableActual(readableActual), messages(messages) {}
