## Convert a Value to a literal type in C.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes, AdvancedStringTypes %>\
<%! from tested.serialisation import as_basic_type %>\
<%! from json import dumps %>\
<%page args="value" />\
<%!
    def escape_char(text):
        return text.replace("'", "\\'")
%>\
## First, add support for the advanced types.
% if value.type == AdvancedSequenceTypes.ARRAY:
    new <%include file="declaration.mako" args="tp=value.type,value=value"/>{<%include file="value_arguments.mako" args="arguments=value.data"/>}\
% elif value.type == AdvancedStringTypes.CHAR:
    (char) '${escape_char(dumps(value.data)[1:-1])}'\
% elif value.type == AdvancedNumericTypes.INT_16:
    ((short) ${value.data})\
% elif value.type == AdvancedNumericTypes.U_INT_16:
    ((unsigned short) ${value.data})\
% elif value.type == AdvancedNumericTypes.INT_64:
    ${value.data}L\
% elif value.type == AdvancedNumericTypes.U_INT_64:
    ${value.data}UL\
% elif value.type == AdvancedNumericTypes.U_INT_32:
    ${value.data}U\
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic,original=value" />\
% endif
