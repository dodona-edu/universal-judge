## Convert a Value to a literal type in C.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes %>
<%! from tested.serialisation import as_basic_type %>
<%page args="value" />
## First, add support for the advanced types.
% if value.type == AdvancedSequenceTypes.ARRAY:
    new <%include file="declaration.mako" args="tp=value.type,value=value"/>{<%include file="value_arguments.mako" args="arguments=value.data"/>}\
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>
    <%include file="value_basic.mako" args="value=basic" />
% endif
