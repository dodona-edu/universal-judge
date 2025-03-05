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
    std::vector<Message*> messages;
    bool result;
    std::string readableExpected;
    std::string readableActual;

    EvaluationResult(std::size_t nrOfMessages);
    ~EvaluationResult();
};

#endif //EVALUATION_RESULT_H
