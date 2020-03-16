## Convert a Value to a literal type in Python.
<%! from serialisation import SequenceTypes, BooleanTypes, StringTypes, NumericTypes, ObjectTypes, NothingTypes  %>
<%page args="value" />
% if value.type == SequenceTypes.SEQUENCE:
    [\
    % for item in value.data:
        <%include file="literal.mako" args="value=item" />
        % if not loop.last:
            , \
        % endif
    % endfor
    ]\
% elif value.type == SequenceTypes.SET:
    {\
    % for item in value.data:
        <%include file="literal.mako" args="value=item" />
        % if not loop.last:
            , \
        % endif
    % endfor
    }\
% elif value.type == BooleanTypes.BOOLEAN:
    ${str(value.data)}\
% elif value.type == StringTypes.TEXT:
    "${value.data}"\
% elif value.type == NumericTypes.INTEGER or value.type == NumericTypes.RATIONAL or value.type == StringTypes.LITERAL:
    ${value.data}\
% elif value.type == ObjectTypes.OBJECT:
    {\
    % for key, item in value.data.items():
        "${key}": \
        <%include file="literal.mako" args="value=item" />
        % if not loop.last:
            , \
        % endif
    % endfor
    }\
% elif value.type == NothingTypes.NOTHING:
    None\
% endif
