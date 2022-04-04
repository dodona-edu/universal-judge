## Convert a Value to a literal type in Kotlin.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes, AdvancedStringTypes %>\
<%! from tested.serialisation import as_basic_type, SpecialNumbers %>\
<%! from json import dumps %>\
<%page args="value" />\
<%!
    def escape_char(text):
        return text.replace("'", "\\'")
%>\
## First, add support for the advanced types in Kotlin.
% if value.type == AdvancedSequenceTypes.ARRAY:
    arrayOf(<%include file="value_arguments.mako" args="arguments=value.data"/>)\
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
% elif value.type == AdvancedNumericTypes.U_INT_32:
    ${value.data}U\
% elif value.type == AdvancedNumericTypes.U_INT_8:
    (${value.data}).toUByte()\
% elif value.type == AdvancedNumericTypes.INT_8:
    (${value.data}).toByte()\
% elif value.type == AdvancedNumericTypes.INT_16:
    (${value.data}).toShort()\
% elif value.type == AdvancedNumericTypes.U_INT_16:
    (${value.data}).toUShort()\
% elif value.type == AdvancedNumericTypes.INT_64:
    ${value.data}L\
% elif value.type == AdvancedNumericTypes.U_INT_64:
    ${value.data}UL\
% elif value.type == AdvancedNumericTypes.BIG_INT:
    BigInteger("${value.data}")\
% elif value.type in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    % if not isinstance(value.data, SpecialNumbers):
        BigDecimal("${value.data}")\
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
