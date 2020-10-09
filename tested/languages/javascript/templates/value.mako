## Convert a literal into JavaScript.
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes  %>\
<%! from tested.serialisation import as_basic_type %>\
<%page args="value" />\
## First, add support for the advanced types in Python.
% if value.type == AdvancedSequenceTypes.TUPLE:
    <% assert False, "tuples are not supported in JavaScript" %>\
% elif value.type == AdvancedNumericTypes.DOUBLE_EXTENDED:
    <% assert False, "extended doubles are not supported in Javascript" %>\
% elif value.type == AdvancedNumericTypes.FIXED_PRECISION:
    <% assert False, "fixed precision floats are not supported in Javascript" %>\
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif