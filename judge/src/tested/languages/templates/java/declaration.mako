## Convert a Value to a type.
<%! from tested.serialisation import VariableType, as_basic_type, resolve_to_basic %>
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes  %>
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>
<%page args="tp,value=None" />
% if tp == AdvancedSequenceTypes.ARRAY:
    <% type_ = value.get_content_type() %>
    <% assert value is not None, "Value is needed for arrays!" %>
    <%include file="declaration.mako" args="tp=type_"/>[]
% elif tp in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT, AdvancedNumericTypes.DOUBLE_EXTENDED):
    BigInteger\
% elif tp in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    BigDecimal\
% elif tp == AdvancedNumericTypes.INT_8:
    byte\
% elif tp in (AdvancedNumericTypes.U_INT_8, AdvancedNumericTypes.INT_16):
    short\
% elif tp in (AdvancedNumericTypes.U_INT_16, AdvancedNumericTypes.INT_32):
    int\
% elif tp in (AdvancedNumericTypes.U_INT_32, AdvancedNumericTypes.INT_64):
    long\
% elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
    float\
% else:
    <% basic = resolve_to_basic(tp) %>
    % if basic == BasicSequenceTypes.SEQUENCE:
        List<Object>\
    % elif basic == BasicSequenceTypes.SET:
        Set<Object>\
    % elif basic == BasicBooleanTypes.BOOLEAN:
        boolean\
    % elif basic == BasicStringTypes.TEXT:
        String\
    % elif basic == BasicStringTypes.CHAR:
        char\
    % elif basic == BasicNumericTypes.INTEGER:
        long\
    % elif basic == BasicNumericTypes.RATIONAL:
        double\
    % elif basic == BasicObjectTypes.MAP:
        Map<Object, Object>\
    % elif basic == BasicNothingTypes.NOTHING:
        Object\
    % endif
% endif
