## Convert a literal into JavaScript.
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes, AdvancedNothingTypes %>\
<%! from tested.serialisation import as_basic_type %>\
<%page args="value" />\
## First, add support for the advanced types in JavaScript.
% if value.type == AdvancedNothingTypes.UNDEFINED:
    undefined\
% elif value.type == AdvancedNumericTypes.DOUBLE_EXTENDED:
    <% assert False, "extended doubles are not supported in Javascript" %>\
% elif value.type == AdvancedNumericTypes.FIXED_PRECISION:
    <% assert False, "fixed precision floats are not supported in Javascript" %>\
% elif value.type in (AdvancedNumericTypes.INT_64, AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
    BigInt("${value.data}")
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif
