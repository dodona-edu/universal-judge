## Convert a Value to a literal type in C#.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%! from tested.serialisation import SpecialNumbers, Value %>\
<%! from json import dumps %>\
<%page args="value" />\
% if value.type == BasicNumericTypes.INTEGER:
    ${value.data}\
% elif value.type == BasicNumericTypes.REAL:
    % if not isinstance(value.data, SpecialNumbers):
        ${value.data}\
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        Double.NaN\
    % elif value.data == SpecialNumbers.POS_INFINITY:
        Double.PositiveInfinity\
    % else:
        Double.NegativeInfinity\
    % endif
% elif value.type == BasicStringTypes.TEXT:
    ${dumps(value.data)}\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ${str(value.data).lower()}\
% elif value.type == BasicNothingTypes.NOTHING:
    null\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    new <%include file="declaration.mako" args="tp=value.type,value=value"/>() { <%include file="value_arguments.mako" args="arguments=value.data" /> }\
% elif value.type == BasicSequenceTypes.SET:
    new <%include file="declaration.mako" args="tp=value.type,value=value"/>() { <%include file="value_arguments.mako" args="arguments=value.data" /> }\
% elif value.type == BasicObjectTypes.MAP:
    new <%include file="declaration.mako" args="tp=value.type,value=value"/>() {\
    % for pair in value.data:
        {\
        <%include file="statement.mako" args="statement=pair.key" />, \
        <%include file="statement.mako" args="statement=pair.value" />}\
        % if not loop.last:
            , \
        % endif
    % endfor
    }\
% endif
