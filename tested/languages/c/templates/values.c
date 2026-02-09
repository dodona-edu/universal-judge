#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "values.h"

#define format(name, x) "{\"type\": " #name ", \"data\":" #x "}"

// Escape string characters
// Idea: https://stackoverflow.com/questions/3201451/how-to-convert-a-c-string-into-its-escaped-version-in-c
char* escape(const char* buffer){
    int l = strlen(buffer);
    char esc_char[]= { '\a', '\b', '\f', '\n', '\r', '\t', '\v', '\\', '\"' };
    char essc_str[]= {  'a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\"' };
    char *dest = (char*) calloc(l * 4 + 1, sizeof(char));
    char *ptr = dest;
    for (int i = 0; i < l; i++) {
        if (((unsigned char) buffer[i]) > 127) {
            unsigned char value = (unsigned char) buffer[i];
            *ptr++ = '0';
            *ptr++ = 'x';
            char upper = value / 16;
            *ptr++ = upper < 10 ? upper + '0' : upper - 10 + 'A';
            char lower = value % 16;
            *ptr++ = lower < 10 ? lower + '0' : lower - 10 + 'A';
        } else {
            int j = 0;
            for (; j < 9; j++) {
                if (buffer[i] == esc_char[j]) {
                    *ptr++ = '\\';
                    *ptr++ = essc_str[j];
                    break;
                }
            }
            if (j >= 9) {
                *ptr++ = buffer[i];
            }
        }
    }
    *ptr = '\0';
    return dest;
}

void write_bool(FILE* out, bool value) {
    const char* asString = format("boolean", %s);
    fprintf(out, asString, value ? "true" : "false");
}

void write_char(FILE* out, char value) {
    const char* asString = format("char", "%s");
    char buffer[2] = {value, '\0'};
    char* result = escape(buffer);
    fprintf(out, asString, result);
    free(result);
}

void write_signed_char(FILE* out, signed char value) {
    const char* asString = format("int8", %d);
    fprintf(out, asString, ((int) value));
}

void write_unsigned_char(FILE* out, unsigned char value) {
    const char* asString = format("uint8", %u);
    fprintf(out, asString, ((unsigned int) value));
}

void write_sint(FILE* out, short int value) {
    const char* asString = format("int16", %d);
    fprintf(out, asString, value);
}

void write_usint(FILE* out, unsigned short int value) {
    const char* asString = format("uint16", %u);
    fprintf(out, asString, value);
}

void write_int(FILE* out, int value) {
    const char* asString = format("int32", %d);
    fprintf(out, asString, value);
}

void write_uint(FILE* out, unsigned int value) {
    const char* asString = format("uint32", %u);
    fprintf(out, asString, value);
}

void write_long(FILE* out, long value) {
    const char* asString = format("int64", %li);
    fprintf(out, asString, value);
}

void write_ulong(FILE* out, unsigned long value) {
    const char* asString = format("uint64", %lu);
    fprintf(out, asString, value);
}

void write_llong(FILE* out, long long value) {
    const char* asString = format("integer", %ll);
    fprintf(out, asString, value);
}

void write_ullong(FILE* out, unsigned long long value) {
    const char* asString = format("bigint", %ull);
    fprintf(out, asString, value);
}

void write_float(FILE* out, float value) {
    if (isnan(value)) {
        const char* asString = format("single_precision", %s);
        fprintf(out, asString, "\"nan\"");
    } else if (isinf(value)) {
        const char* asString = format("single_precision", %s);
        if (value < 0) {
          fprintf(out, asString, "\"-inf\"");
        } else {
          fprintf(out, asString, "\"inf\"");
        }
    } else {
        const char* asString = format("single_precision", %f);
        fprintf(out, asString, value);
    }
}

void write_double(FILE* out, double value) {
    if (isnan(value)) {
        const char* asString = format("double_precision", %s);
        fprintf(out, asString, "\"nan\"");
    } else if (isinf(value)) {
        const char* asString = format("double_precision", %s);
        if (value < 0) {
          fprintf(out, asString, "\"-inf\"");
        } else {
          fprintf(out, asString, "\"inf\"");
        }
    } else {
        const char* asString = format("double_precision", %lf);
        fprintf(out, asString, value);
    }
}

void write_ldouble(FILE* out, long double value) {
    if (isnan(value)) {
        const char* asString = format("double_extended", %s);
        fprintf(out, asString, "\"nan\"");
    } else if (isfinite(value)) {
        const char* asString = format("double_extended", %Lf);
        fprintf(out, asString, value);
    } else if (value < 0) {
        const char* asString = format("double_extended", %s);
        fprintf(out, asString, "\"-inf\"");
    } else {
        const char* asString = format("double_extended", %s);
        fprintf(out, asString, "\"inf\"");
    }
}

void write_string(FILE* out, const char* value) {
    const char* asString = format("text", "%s");
    char* result = escape(value);
    fprintf(out, asString, result);
    free(result);
}

void write_unknown(FILE* out, void* value) {
    const char* asString = format("unknown", "%s");
    fprintf(out, asString, "?");
}

void write_void(FILE* out, void* value) {
    const char* asString = format("nothing", %s);
    fprintf(out, asString, "null");
}

void write_evaluated(FILE* out, EvaluationResult* result) {

    // Count the size of the string we need for each message object.
    size_t messageLength = 0;
    for (size_t i = 0; i < result->nrOfMessages; ++i) {
        Message* message = result->messages[i];
        if (message->permission == NULL) {
            char* template = "{'description': '', 'format': ''}";
            messageLength += strlen(template);
        } else {
            char* template = "{'description': '', 'format': '', 'permission': ''}";
            messageLength += strlen(template);
            messageLength += strlen(message->permission);
        }
        messageLength += strlen(message->description);
        if (message->format == NULL) {
            messageLength += 4;
        } else {
            messageLength += strlen(message->format);
        }
    }

    size_t totalLength = messageLength + result->nrOfMessages;
    char* concatMessages = calloc(totalLength, sizeof(char));
    size_t nextMessage = 0;

    for (size_t j = 0; j < result->nrOfMessages; ++j) {
        Message* message = result->messages[j];
        int added;
        if (message->permission == NULL) {
            char* template = "{\"description\": \"%s\", \"format\": \"%s\"}";
            added = sprintf(&concatMessages[nextMessage], template, message->description, message->format);
        } else {
            char* template = "{\"description\": \"%s\", \"format\": \"%s\", \"permission\": \"%s\"}";
            added = sprintf(&concatMessages[nextMessage], template, message->description, message->format,
                            message->permission);
        }
        nextMessage += added;
        concatMessages[nextMessage] = ',';
        nextMessage++;
    }
    // The last comma is not needed, so use it as null-delimiter.
    concatMessages[nextMessage - 1] = '\0';

    const char* value = "{"
                        "\"result\": %s, "
                        "\"readable_expected\": \"%s\", "
                        "\"readable_actual\": \"%s\", "
                        "\"messages\": [%s]"
                        "}";
    const char* resulting = result->result ? "true" : "false";
    fprintf(out, value, resulting, result->readableExpected, result->readableActual, concatMessages);
    free(concatMessages);
    free_result(result);
}
