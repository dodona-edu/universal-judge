#ifndef EVALUATION_RESULT_H
#define EVALUATION_RESULT_H

#include <iostream>
#include <vector>
#include <string>

using namespace std;

class Message {
public:
    string description;
    string format;
    string permission;

    Message(const string &description, const string &format = "text", const string &permission = "");
};

class EvaluationResult {
public:
    vector<Message*> messages;
    bool result;
    string readableExpected;
    string readableActual;

    EvaluationResult(size_t nrOfMessages);
    ~EvaluationResult();
};

#endif //EVALUATION_RESULT_H
