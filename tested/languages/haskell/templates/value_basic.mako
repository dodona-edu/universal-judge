<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%! from tested.serialisation import SpecialNumbers %>\
<%! from json import dumps %>\
<%page args="value" />\
% if value.type == BasicNumericTypes.INTEGER:
    ${value.data} :: Int\
% elif value.type == BasicNumericTypes.RATIONAL:
    % if not isinstance(value.data, SpecialNumbers):
        ${value.data} \
    % elif value.data == SpecialNumbers.NOT_A_NUMBER:
        (0/0) \
    % elif value.data == SpecialNumbers.POS_INFINITY:
        (1/0) \
    % else:
        (-1/0) \
    % endif
    :: Double\
% elif value.type == BasicStringTypes.TEXT:
    ${dumps(value.data)}\
% elif value.type == BasicBooleanTypes.BOOLEAN:
    ${str(value.data)}\
% elif value.type == BasicNothingTypes.NOTHING:
    Nothing :: Maybe Integer\
% elif value.type == BasicSequenceTypes.SEQUENCE:
    [<%include file="value_arguments.mako" args="arguments=value.data" />]\
% endif
