#ifndef EVALUATION_RESULT_H
#define EVALUATION_RESULT_H

#include <iostream>
#include <vector>
#include <string>

using namespace std;

// Define the Message structure
struct Message {
    string description;
    string format;
    string permission;

    Message(const string &desc, const string &fmt, const string &perm);
};

// Define the EvaluationResult structure
struct EvaluationResult {
    vector<Message*> messages;
    bool result;
    string readableExpected;
    string readableActual;

    EvaluationResult(size_t nrOfMessages);
    ~EvaluationResult();
};


// Function to create an EvaluationResult object
EvaluationResult* create_result(size_t nrOfMessages);

// Function to free an EvaluationResult object
void free_result(EvaluationResult *result);

// Function to create a Message object
Message* create_message(const string &description, const string &format, const string &permission);

// Function to free a Message object
void free_message(Message *message);

#endif //EVALUATION_RESULT_H
