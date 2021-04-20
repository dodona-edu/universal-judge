## Convert a Value to a literal type in Java.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes, AdvancedStringTypes %>\
<%! from tested.serialisation import as_basic_type, SpecialNumbers %>\
<%! from json import dumps %>\
<%page args="value" />\
<%!
    def escape_char(text):
        return text.replace("'", "\\'")
%>\
## First, add support for the advanced types in Java.
% if value.type == AdvancedSequenceTypes.ARRAY:
    new <%include file="declaration.mako" args="tp=value.type,value=value"/>{<%include file="value_arguments.mako" args="arguments=value.data"/>}\
% elif value.type == AdvancedNumericTypes.SINGLE_PRECISION:
    % if not isinstance(value.data, SpecialNumbers):
        ${value.data}f\
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        Float.NaN\
    % elif value.data == SpecialNumbers.POS_INFINITY:
        Float.POSITIVE_INFINITY\
    % else:
        Float.NEGATIVE_INFINITY\
    % endif
% elif value.type == AdvancedNumericTypes.INT_8:
    (byte) ${value.data}\
% elif value.type in (AdvancedNumericTypes.U_INT_8, AdvancedNumericTypes.INT_16):
    (short) ${value.data}\
% elif value.type in (AdvancedNumericTypes.U_INT_16, AdvancedNumericTypes.INT_32):
    ${value.data}\
% elif value.type in (AdvancedNumericTypes.U_INT_32, AdvancedNumericTypes.INT_64):
    ${value.data}L\
% elif value.type in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
    new BigInteger("${value.data}")\
% elif value.type in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    % if not isinstance(value.data, SpecialNumbers):
        new BigDecimal("${value.data}")\
    % else:
        <% raise ValueError("Special numbers not supported for BigDecimal") %>\
    % endif
% elif value.type == AdvancedStringTypes.CHAR:
    '${escape_char(dumps(value.data)[1:-1])}'\
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif