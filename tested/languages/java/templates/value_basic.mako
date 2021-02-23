## Convert a Value to a literal type in Java.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%page args="value" />\
<%!
    def escape_string(text):
        return text.replace('"', '\\"')
    def escape_char(text):
        return text.replace("'", "\\'")
%>\
% if value.type == BasicNumericTypes.INTEGER:
    ${value.data}\
% elif value.type == BasicNumericTypes.RATIONAL:
    ${value.data}\
% elif value.type == BasicStringTypes.TEXT:
    "${escape_string(value.data)}"\
% elif value.type == BasicStringTypes.CHAR:
    '${escape_char(value.data)}'\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ${str(value.data).lower()}\
% elif value.type == BasicNothingTypes.NOTHING:
    null\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    List.of(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == BasicSequenceTypes.SET:
    Set.of(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == BasicObjectTypes.MAP:
    Map.of(\
    % for pair in value.data:
        <%include file="statement.mako" args="statement=pair.key" />, \
        <%include file="statement.mako" args="statement=pair.value" />\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif