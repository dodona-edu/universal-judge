## Convert a Value to a literal type in Java.
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>
<%page args="value" />
% if value.type in (BasicNumericTypes.INTEGER, BasicNumericTypes.RATIONAL):
    ${value.data}\
% elif value.type == BasicStringTypes.TEXT:
    "${value.data}"\
% elif value.type == BasicStringTypes.CHAR:
    '${value.data}'\
% elif value.type == BooleanTypes.BOOLEAN:
    ${str(value.data).lower()}\
% elif value.type == NothingTypes.NOTHING:
    null\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    List.of(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == SequenceTypes.SET:
    Set.of(<%include file="value_arguments.mako" args="arguments=value.data" />)\
% elif value.type == ObjectTypes.MAP:
    Map.of(\
    % for key, item in value.data.items():
        "${key}", <%include file="expression.mako" args="value=item" />
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif
