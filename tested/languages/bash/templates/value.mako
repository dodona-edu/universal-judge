## Convert a Value to a literal type in BASH.
<%! from tested.datatypes import BasicStringTypes, AdvancedStringTypes %>\
<%page args="value" />\
% if value.type in (BasicStringTypes.TEXT, AdvancedStringTypes.CHAR):
    "${value.data.replace("\\", "\\\\").replace('"', '\\"')}"\
% endif
