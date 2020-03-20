## Convert a Value to a literal type in Python.
<%! from serialisation import SequenceTypes, BooleanTypes, StringTypes, NumericTypes, ObjectTypes, NothingTypes  %>
<%page args="value" />
% if value.type == BooleanTypes.BOOLEAN:
    ${str(value.data).lower()}\
% elif value.type == StringTypes.TEXT:
    "${value.data}"\
% elif value.type == NumericTypes.INTEGER or value.type == NumericTypes.RATIONAL or value.type == StringTypes.IDENTIFIER:
    ${value.data}\
% elif value.type == NothingTypes.NOTHING:
    NULL\
% endif
