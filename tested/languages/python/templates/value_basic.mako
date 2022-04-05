## Convert a Value to a literal type in Python.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%! from tested.serialisation import SpecialNumbers %>\
<%page args="value" />\
% if value.type in (BasicNumericTypes.INTEGER, BasicNumericTypes.REAL):
    % if not isinstance(value.data, SpecialNumbers):
        ${value.data}\
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        float('nan')\
    % elif value.data == SpecialNumbers.POS_INFINITY:
        float('inf')\
    % else:
        float('-inf')\
    % endif
% elif value.type == BasicStringTypes.TEXT:
    ${repr(value.data)}\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ${str(value.data)}\
% elif value.type == BasicNothingTypes.NOTHING:
    None\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    [<%include file="value_arguments.mako" args="arguments=value.data" />]\
% elif value.type == BasicSequenceTypes.SET:
    {<%include file="value_arguments.mako" args="arguments=value.data" />}\
% elif value.type == BasicObjectTypes.MAP:
    {\
    % for pair in value.data:
        <%include file="statement.mako" args="statement=pair.key" />: \
        <%include file="statement.mako" args="statement=pair.value" />\
        % if not loop.last:
            , \
        % endif
    % endfor
    }\
% endif
