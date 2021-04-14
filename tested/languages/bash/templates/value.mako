## Convert a Value to a literal type in BASH.
<%! from tested.datatypes import BasicStringTypes %>\
<%page args="value" />\
% if value.type == BasicStringTypes.TEXT:
    ${repr(value.data)}\
% endif
