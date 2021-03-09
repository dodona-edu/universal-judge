## Convert a Value to a literal type in Java.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes, AdvancedStringTypes %>\
<%! from tested.serialisation import as_basic_type %>\
<%! from tested.utils import get_args %>\
<%! from json import dumps %>\
<%page args="value" />\
<%!
    def escape_char(text):
        return text.replace("'", "\\'")
%>\
## First, add support for the advanced types in Java.
% if value.type == AdvancedSequenceTypes.TUPLE:
    (<%include file="value_arguments.mako" args="arguments=value.data"/>)\
% elif isinstance(value.type, get_args(AdvancedNumericTypes)):
    ${value.data} :: <%include file="declaration.mako" args="tp=value.type" />\
% elif value.type == AdvancedStringTypes.CHAR:
    '${escape_char(dumps(value.data)[1:-1])}'\
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif