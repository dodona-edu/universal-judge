## Convert a Value to a literal type in Kotlin.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%page args="value" />\
<%!
    def escape_string(text):
        return text.replace('"', '\\"')
    def escape_char(text):
        return text.replace("'", "\\'")
%>\
% if value.type == BasicNumericTypes.INTEGER:
    ## Basic heuristic for long/int
    % if len(str(value.data)) >= 10:
            ${value.data}L\
    % else:
            ${value.data}\
    % endif
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
    listOf(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == BasicSequenceTypes.SET:
    setOf(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == BasicObjectTypes.MAP:
    mapOf(\
    % for key, item in value.data.items():
        Pair("${key}", <%include file="statement.mako" args="statement=item" />)\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif