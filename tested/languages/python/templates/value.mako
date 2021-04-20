## Convert a Value to a literal type in Python.
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes  %>\
<%! from tested.serialisation import as_basic_type %>\
<%page args="value" />\
## First, add support for the advanced types in Python.
% if value.type == AdvancedSequenceTypes.TUPLE:
    (<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    % if not isinstance(value.data, SpecialNumbers):
        Decimal("${value.data}")\
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        Decimal(float('nan'))\
    % elif value.data == SpecialNumbers.POS_INFINITY:
        Decimal(float('inf'))\
    % else:
        Decimal(float('-inf'))\
    % endif
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif