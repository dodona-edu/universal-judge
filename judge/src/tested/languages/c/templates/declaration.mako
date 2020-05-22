## Convert a Value to a type.
<%! from tested.serialisation import BasicBooleanTypes, BasicNumericTypes, BasicStringTypes, BasicNothingTypes  %>\
<%page args="value" />\
% if value.type == BasicBooleanTypes.BOOLEAN:
    boolean\
% elif value.type == BasicStringTypes.TEXT:
    char*\
% elif value.type == BasicNumericTypes.INTEGER:
    long long\
% elif value.type == BasicNumericTypes.RATIONAL:
    double\
% elif value.type == BasicNothingTypes.NOTHING or value.type == BasicStringTypes.ANY:
    void*\
% endif