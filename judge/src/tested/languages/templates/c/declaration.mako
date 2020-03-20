## Convert a Value to a type.
<%! from tested.serialisation import BooleanTypes, StringTypes, NumericTypes, NothingTypes  %>
<%page args="value" />
% if value.type == BooleanTypes.BOOLEAN:
    boolean\
% elif value.type == StringTypes.TEXT:
    char*\
% elif value.type == NumericTypes.INTEGER:
    long long\
% elif value.type == NumericTypes.RATIONAL:
    double\
% elif value.type == NothingTypes.NOTHING or value.type == StringTypes.UNKNOWN:
    void*\
% endif
