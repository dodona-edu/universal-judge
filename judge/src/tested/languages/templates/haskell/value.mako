## Convert a Value to a literal type in Java.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes %>
<%! from tested.serialisation import as_basic_type %>
<%! from tested.utils import get_args %>
<%page args="value" />
## First, add support for the advanced types in Java.
% if value.type == AdvancedSequenceTypes.TUPLE:
    (<%include file="value_arguments.mako" args="arguments=value.data"/>)\
% elif isinstance(value.type, get_args(AdvancedNumericTypes)):
    ${data.value} :: <%include file="declaration.mako" args="tp=data.type" />
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>
    <%include file="value_basic.mako" args="value=basic" />
% endif
