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
        ${value.data}\
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        Float.NaN\
    % elif value.data == SpecialNumbers.POS_INFINITY:
        Float.POSITIVE_INFINITY\
    % else:
        Float.NEGATIVE_INFINITY\
    % endif
% elif value.type in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
    BigInteger("${value.data}")\
% elif value.type in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    BigDecimal("${value.data}")\
% elif value.type == AdvancedStringTypes.CHAR:
    '${escape_char(dumps(value.data)[1:-1])}'\
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif