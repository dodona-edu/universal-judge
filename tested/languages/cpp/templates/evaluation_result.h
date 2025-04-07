#ifndef EVALUATION_RESULT_H
#define EVALUATION_RESULT_H

#include <iostream>
#include <vector>
#include <string>

class Message {
public:
    std::string description;
    std::string format;
    std::string permission;

    Message(const std::string &description, const std::string &format = "text", const std::string &permission = "");
};

class EvaluationResult {
public:
    bool result;
    std::string readableExpected;
    std::string readableActual;
    std::vector<Message> messages;

    EvaluationResult(const bool &result, const std::string &readableExpected = "", const std::string &readableActual = "", const std::vector<Message> &messages = std::vector<Message>());
};

#endif //EVALUATION_RESULT_H
