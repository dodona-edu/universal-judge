## Convert a base type literal into Javascript.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%page args="value" />\
<%!
    def escape_string(text):
        return text.replace('"', '\\"')
%>\
% if value.type in (BasicNumericTypes.INTEGER, BasicNumericTypes.RATIONAL):
    ${value.data}\
% elif value.type in (BasicStringTypes.TEXT, BasicStringTypes.CHAR):
    "${escape_string(value.data)}"\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ## JavaScript Boolean literals (true, false) are lowercase (pascal case in Python)
    ${str(value.data).lower()}\
% elif value.type == BasicNothingTypes.NOTHING:
    null\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    [<%include file="value_arguments.mako" args="arguments=value.data" />]\
% elif value.type == BasicSequenceTypes.SET:
    new Set([\
        <%include file="value_arguments.mako" args="arguments=value.data" />\
    ])\
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
% else:
    <% raise ValueError('Invalid literal: ' + repr(value)) %>\
% endif