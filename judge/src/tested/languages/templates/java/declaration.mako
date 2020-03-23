## Convert a Value to a type.
<%! from tested.serialisation import VariableType, as_basic_type %>
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes  %>
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>
<%page args="type" />
% if value.type == AdvancedSequenceTypes.ARRAY:
    <% type_ = value.get_content_type() %>
    ${type}[]\
% elif value.type in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT, AdvancedNumericTypes.DOUBLE_EXTENDED):
    BigInteger\
% elif value.type in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    BigDecimal\
% elif value.type == AdvancedNumericTypes.INT_8:
    byte\
% elif value.type in (AdvancedNumericTypes.U_INT_8, AdvancedNumericTypes.INT_16):
    short\
% elif value.type in (AdvancedNumericTypes.U_INT_16, AdvancedNumericTypes.INT_32):
    int\
% elif value.type in (AdvancedNumericTypes.U_INT_32, AdvancedNumericTypes.INT_64):
    long\
% elif value.type == AdvancedNumericTypes.SINGLE_PRECISION:
    float\
% else:
    <% basic = as_basic_type(value) %>
    % if value.type == BasicSequenceTypes.SEQUENCE:
        List\
    % elif value.type == BasicSequenceTypes.SET:
        Set\
    % elif value.type == BasicBooleanTypes.BOOLEAN:
        boolean\
    % elif value.type == BasicStringTypes.TEXT:
        String\
    % elif value.type == BasicStringTypes.CHAR:
        char\
    % elif value.type == BasicNumericTypes.INTEGER:
        long\
    % elif value.type == BasicNumericTypes.RATIONAL:
        double\
    % elif value.type == BasicObjectTypes.MAP:
        Map\
    % elif value.type == BasicNothingTypes.NOTHING:
        Object\
    % endif
% endif
