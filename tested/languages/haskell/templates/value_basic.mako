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
    ${value.data} :: Int\
% endif
% if value.type == BasicNumericTypes.RATIONAL:
    ${value.data} :: Double\
% elif value.type == BasicStringTypes.TEXT:
    "${escape_string(value.data)}"\
% elif value.type == BasicStringTypes.CHAR:
    '${escape_char(value.data)}'\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ${str(value.data)}\
% elif value.type == BasicNothingTypes.NOTHING:
    Nothing :: Maybe Integer\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    [<%include file="value_arguments.mako" args="arguments=value.data" />]\
% endif