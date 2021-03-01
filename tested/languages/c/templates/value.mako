## Convert a Value to a literal type in C.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes, AdvancedStringTypes %>\
<%! from tested.serialisation import as_basic_type %>\
<%page args="value" />\
<%!
    def escape_char(text):
        return text.replace("'", "\\'")
%>\
## First, add support for the advanced types.
% if value.type == AdvancedSequenceTypes.ARRAY:
    new <%include file="declaration.mako" args="tp=value.type,value=value"/>{<%include file="value_arguments.mako" args="arguments=value.data"/>}\
% elif value.type == AdvancedStringTypes.CHAR:
    (char) '${escape_char(value.data)}'\
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif