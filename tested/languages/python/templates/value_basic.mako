## Convert a Value to a literal type in Python.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%page args="value" />\
% if value.type in (BasicNumericTypes.INTEGER, BasicNumericTypes.RATIONAL):
    ${value.data}\
% elif value.type in (BasicStringTypes.TEXT, BasicStringTypes.CHAR):
    ${repr(value.data).replace("\\\\", "\\")}\
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