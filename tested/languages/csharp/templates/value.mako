## Convert a Value to a literal type in Java.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes, AdvancedStringTypes %>\
<%! from tested.serialisation import as_basic_type, SpecialNumbers %>\
<%! from json import dumps %>\
<%page args="value,nt=None" />\
## First, add support for the advanced types in C#.
% if value.type == AdvancedSequenceTypes.ARRAY:
    new <%include file="declaration.mako" args="tp=value.type,value=value"/>{<%include file="value_arguments.mako" args="arguments=value.data"/>}\
% elif value.type == AdvancedSequenceTypes.TUPLE:
    (<%include file="value_arguments.mako" args="arguments=value.data"/>)\
% elif value.type == AdvancedNumericTypes.SINGLE_PRECISION:
    % if not isinstance(value.data, SpecialNumbers):
        ${value.data}f\
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        Single.NaN\
    % elif value.data == SpecialNumbers.POS_INFINITY:
        Single.PositiveInfinity\
    % else:
        Single.NegativeInfinity\
    % endif
% elif value.type == AdvancedNumericTypes.U_INT_8:
    (byte) ${value.data}\
% elif value.type == AdvancedNumericTypes.INT_8:
    (sbyte) ${value.data}\
% elif value.type == AdvancedNumericTypes.INT_16:
    (short) ${value.data}\
% elif value.type == AdvancedNumericTypes.U_INT_16:
    (ushort) ${value.data}\
% elif value.type == AdvancedNumericTypes.INT_32:
    (int) ${value.data}\
% elif value.type == AdvancedNumericTypes.U_INT_32:
    (uint) ${value.data}\
% elif value.type == AdvancedNumericTypes.INT_64:
    (long) ${value.data}\
% elif value.type == AdvancedNumericTypes.U_INT_64:
    (ulong) ${value.data}\
% elif value.type == AdvancedNumericTypes.BIG_INT:
    BigInteger.Parse("${value.data}")\
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif
