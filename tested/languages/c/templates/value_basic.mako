## Convert a Value to a literal type in Java.
<%! from tested.datatypes import AdvancedNumericTypes, BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%! from tested.serialisation import SpecialNumbers %>\
<%! from json import dumps %>\
<%page args="value,original" />\
% if value.type == BasicNumericTypes.INTEGER:
    ${value.data}\
% elif value.type == BasicNumericTypes.REAL:
    <% suffix = "f" if original.type == AdvancedNumericTypes.SINGLE_PRECISION else "" %>\
    % if not isinstance(value.data, SpecialNumbers):
        ${value.data}${suffix}\
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        nan${suffix}("")\
    % elif value.data == SpecialNumbers.POS_INFINITY:
        % if original.type == AdvancedNumericTypes.DOUBLE_PRECISION:
          ((double) INFINITY)\
        % else:
          INFINITY\
        % endif
    % else:
        % if original.type == AdvancedNumericTypes.DOUBLE_PRECISION:
          ((double) -INFINITY)\
        % else:
          (-INFINITY)\
        % endif
    % endif
% elif value.type == BasicStringTypes.TEXT:
    ${dumps(value.data)}\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    (bool) ${str(value.data).lower()}\
% elif value.type == BasicNothingTypes.NOTHING:
    NULL\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    // TODO
    // List.of(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% endif
