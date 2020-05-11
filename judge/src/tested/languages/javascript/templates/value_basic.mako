## Convert a base type literal into Javascript.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>
<%page args="value" />
% if value.type in (BasicNumericTypes.INTEGER, BasicNumericTypes.RATIONAL):
    ${value.data}\
% elif value.type in (BasicStringTypes.TEXT, BasicStringTypes.CHAR):
    ## TODO: should prefer double quotes over single quotes
    ${repr(value.data)}\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ## JavaScript Boolean literals (true, false) are lowercase (pascal case in Python)
    ${str(value.data)/lower()}\
% elif value.type == BasicNothingTypes.NOTHING:
    Null\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    [<%include file="value_arguments.mako" args="arguments=value.data" />]\
% elif value.type == BasicSequenceTypes.SET:
    new Set([\
        {<%include file="value_arguments.mako" args="arguments=value.data" />}\
    ])\
% elif value.type == BasicObjectTypes.MAP:
    {\
    % for key, item in value.data.items():
        "${key}": <%include file="statement.mako" args="statement=item" />\
        % if not loop.last:
            , \
        % endif
    % endfor
    }\
% else:
    <% raise ValueError(f'Invalid literal: {repr(value)}') %>
% endif
