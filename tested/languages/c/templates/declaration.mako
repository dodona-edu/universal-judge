## Convert a Value to a type.
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes, AdvancedStringTypes  %>\
<%! from tested.serialisation import VariableType, as_basic_type, resolve_to_basic, Value %>\
<%! from tested.serialisation import BasicBooleanTypes, BasicNumericTypes, BasicStringTypes, BasicNothingTypes  %>\
<%page args="tp,value" />\
% if isinstance(tp, VariableType):
    ${tp.data}\
% elif tp == AdvancedNumericTypes.BIG_INT:
    long long\
% elif tp == AdvancedNumericTypes.U_INT_64:
    unsigned long\
% elif tp == AdvancedNumericTypes.INT_64:
    long\
% elif tp == AdvancedNumericTypes.U_INT_32:
    unsigned int\
% elif tp == AdvancedNumericTypes.INT_32:
    int\
% elif tp == AdvancedNumericTypes.U_INT_16:
    unsigned short int\
% elif tp == AdvancedNumericTypes.INT_16:
    short int\
% elif tp == AdvancedNumericTypes.U_INT_8:
    unsigned char\
% elif tp == AdvancedNumericTypes.INT_8:
    signed char\
% elif tp == AdvancedNumericTypes.DOUBLE_EXTENDED:
    long double\
% elif tp == AdvancedNumericTypes.DOUBLE_PRECISION:
    double\
% elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
    float\
% elif tp == AdvancedStringTypes.CHAR:
    char\
% else:
    <% basic = resolve_to_basic(tp) %>\
    % if basic == BasicBooleanTypes.BOOLEAN:
        bool\
    % elif basic == BasicStringTypes.TEXT:
        char*\
    % elif basic == BasicNumericTypes.INTEGER:
        long long\
    % elif basic == BasicNumericTypes.REAL:
        double\
    % elif basic == BasicNothingTypes.NOTHING or value.type == BasicStringTypes.ANY:
        void*\
    % endif
% endif
