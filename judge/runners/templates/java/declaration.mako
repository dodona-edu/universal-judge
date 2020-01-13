## Convert a Value to a type.
<%! from serialisation import SequenceTypes, BooleanTypes, StringTypes, NumericTypes, ObjectTypes, NothingTypes  %>
<%page args="value" />
% if value.type == SequenceTypes.LIST:
    List\
% elif value.type == SequenceTypes.SET:
    Set\
% elif value.type == BooleanTypes.BOOLEAN:
    boolean\
% elif value.type == StringTypes.TEXT:
    String\
% elif value.type == NumericTypes.INTEGER:
    int\
% elif value.type == NumericTypes.RATIONAL:
    double\
% elif value.type == ObjectTypes.OBJECT:
    Map\
% elif value.type == NothingTypes.NOTHING or value.type == StringTypes.UNKNOWN:
    Object\
% endif