#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "values.h"

#define format(name, x) "{\"type\": " #name ", \"data\":" #x "}"

// Escape string characters
char* escape(const char* buffer){
    int i,j;
    int l = strlen(buffer) + 1;
    char esc_char[]= { '\a','\b','\f','\n','\r','\t','\v','\\','\"','\''};
    char essc_str[]= {  'a', 'b', 'f', 'n', 'r', 't', 'v','\\','\"','\''};
    char *dest = (char*) calloc(l*2, sizeof(char));
    char *ptr = dest;
    for(i=0;i<l;i++){
        for(j=0; j< 10 ;j++){
            if( buffer[i]==esc_char[j] ){
              *ptr++ = '\\';
              *ptr++ = essc_str[j];
                 break;
            }
        }
        if(j >= 10) {
            *ptr++ = buffer[i];
        }
    }
    *ptr='\0';
    return dest;
}

void write_bool(FILE* out, bool value) {
    const char* asString = format("boolean", %s);
    fprintf(out, asString, value ? "true" : "false");
}

void write_char(FILE* out, char value) {
    const char* asString = format("char", "%s");
    char buffer[2];
    sprintf(buffer, "%c", value);
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
    const char* asString = format("int64", %l);
    fprintf(out, asString, value);
}

void write_ulong(FILE* out, unsigned long value) {
    const char* asString = format("uint64", %ul);
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
    const char* asString = format("single_precision", %f);
    fprintf(out, asString, value);
}

void write_double(FILE* out, double value) {
    const char* asString = format("double_precision", %lf);
    fprintf(out, asString, value);
}

void write_ldouble(FILE* out, long double value) {
    const char* asString = format("double_extended", %Lf);
    fprintf(out, asString, value);
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
