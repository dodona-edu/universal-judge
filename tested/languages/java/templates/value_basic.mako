## Convert a Value to a literal type in Java.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%! from tested.serialisation import SpecialNumbers %>\
<%! from json import dumps %>\
<%page args="value" />\
% if value.type == BasicNumericTypes.INTEGER:
    ## Basic heuristic for long numbers
    % if (value.data > (2**31 - 1)) or (value.data < -2**31):
        ${value.data}L\
    % else:
        ${value.data}\
    % endif
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
    List.of(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == BasicSequenceTypes.SET:
    Set.of(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == BasicObjectTypes.MAP:
    Map.ofEntries(\
    % for pair in value.data:
        Map.entry(\
        <%include file="statement.mako" args="statement=pair.key" />, \
        <%include file="statement.mako" args="statement=pair.value" />)\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif
