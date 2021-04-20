## Convert a Value to a literal type in Kotlin.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%! from tested.serialisation import SpecialNumbers %>\
<%! from json import dumps %>\
<%page args="value" />\
% if value.type == BasicNumericTypes.INTEGER:
    ${value.data}\
% elif value.type == BasicNumericTypes.RATIONAL:
    % if not isinstance(value.data, SpecialNumbers):
        ${value.data}\
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        Double.NaN\
    % elif value.data == SpecialNumbers.POS_INFINITY:
        Double.POSITIVE_INFINITY\
    % else:
        Double.NEGATIVE_INFINITY\
    % endif
% elif value.type == BasicStringTypes.TEXT:
    ${dumps(value.data)}\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ${str(value.data).lower()}\
% elif value.type == BasicNothingTypes.NOTHING:
    null\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    listOf(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == BasicSequenceTypes.SET:
    setOf(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == BasicObjectTypes.MAP:
    mapOf(\
    % for pair in value.data:
        Pair(<%include file="statement.mako" args="statement=pair.key" />, \
        <%include file="statement.mako" args="statement=pair.value" />)\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif