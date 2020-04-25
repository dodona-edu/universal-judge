## Convert a Value to a literal type in Java.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>
<%page args="value" />
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
    "${value.data}"\
% elif value.type == BasicStringTypes.CHAR:
    '${value.data}'\
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
    % for key, item in value.data.items():
        "${key}", <%include file="statement.mako" args="statement=item" />
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif
