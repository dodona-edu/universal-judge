## Convert a Value to a literal type in Python.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>
<%page args="value" />
% if value.type in (BasicNumericTypes.INTEGER, BasicNumericTypes.RATIONAL):
    ${value.data}\
% elif value.type in (BasicStringTypes.TEXT, BasicStringTypes.CHAR):
    "${value.data}"\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ${str(value.data)}\
% elif value.type == BasicNothingTypes.NOTHING:
    None\
% elif value.type == SequenceTypes.SEQUENCE:
    [<%include file="value_arguments.mako" args="arguments=value.data" />]\
% elif value.type == SequenceTypes.SET:
    {<%include file="value_arguments.mako" args="arguments=value.data" />}\
% elif value.type == ObjectTypes.MAP:
    {\
    % for key, item in value.data.items():
        "${key}": <%include file="expression.mako" args="value=item" />\
        % if not loop.last:
            , \
        % endif
    % endfor
    }\
% endif
