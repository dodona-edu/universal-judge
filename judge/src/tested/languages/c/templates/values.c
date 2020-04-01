#include <string.h>
#include <stdlib.h>

#include "values.h"

#define format(name, x) "{\"type\": " #name ", \"data\":" #x "}"


void write_bool(FILE* out, bool value) {
    const char* asString = format("boolean", %s);
    fprintf(out, asString, value ? "true" : "false");
}

void write_char(FILE * out, char value) {
    const char* asString = format("integer", %d);
    fprintf(out, asString, value);
}

void write_uchar(FILE * out, unsigned char value) {
    const char* asString = format("integer", %u);
    fprintf(out, asString, value);
}

void write_sint(FILE * out, short int value) {
    const char* asString = format("integer", %d);
    fprintf(out, asString, value);
}

void write_usint(FILE * out, unsigned short int value) {
    const char* asString = format("integer", %u);
    fprintf(out, asString, value);
}

void write_int(FILE * out, int value) {
    const char* asString = format("integer", %d);
    fprintf(out, asString, value);
}

void write_uint(FILE * out, unsigned int value) {
    const char* asString = format("integer", %u);
    fprintf(out, asString, value);
}

void write_long(FILE * out, long value) {
    const char* asString = format("integer", %l);
    fprintf(out, asString, value);
}

void write_ulong(FILE * out, unsigned long value) {
    const char* asString = format("integer", %ul);
    fprintf(out, asString, value);
}

void write_llong(FILE * out, long long value) {
    const char* asString = format("integer", %ll);
    fprintf(out, asString, value);
}

void write_ullong(FILE * out, unsigned long long value) {
    const char* asString = format("integer", %ull);
    fprintf(out, asString, value);
}

void write_float(FILE * out, float value) {
    const char* asString = format("rational", %f);
    fprintf(out, asString, value);
}

void write_double(FILE * out, double value) {
    const char* asString = format("rational", %f);
    fprintf(out, asString, value);
}

void write_ldouble(FILE * out, long double value) {
    const char* asString = format("rational", %Lf);
    fprintf(out, asString, value);
}

void write_string(FILE * out, const char * value) {
    const char* asString = format("text", "%s");
    fprintf(out, asString, value);
}

void write_unknown(FILE * out, void * value) {
    const char* asString = format("unknown", "%s");
    fprintf(out, asString, "?");
}

void send_evaluated(FILE *out, bool result, const char *expected, const char *actual, size_t nrOfMessages, const char **messages) {

    // Find the length of the string we need.
    size_t messageLength = 0;
    for (size_t i = 0; i < nrOfMessages; ++i) {
        const char* message = messages[i];
        messageLength += strlen(message);
        messageLength += 3; // For the comma, and two quotes.
    }

    char* concatMessages = calloc(messageLength, sizeof(char));
    size_t nextMessage = 0;

    for (size_t j = 0; j < nrOfMessages; ++j) {
        concatMessages[nextMessage++] = '"';
        strcpy(&concatMessages[nextMessage], messages[j]);
        nextMessage += strlen(messages[j]);
        concatMessages[nextMessage++] = '"';
        concatMessages[nextMessage++] = ',';
    }
    // The last comma is not needed, so use it as null-delimiter.
    concatMessages[nextMessage - 1] = '\0';

    const char* value = "{"
                        "\"result\": %s, "
                        "\"readable_expected\": \"%s\", "
                        "\"readable_actual\": \"%s\", "
                        "\"messages\": [%s]"
                        "}";
    const char* resulting = result ? "true" : "false";
    fprintf(out, value, resulting, expected, actual, concatMessages);
    free(concatMessages);
}
