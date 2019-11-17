<%! from serialisation import Value, StringTypes %>
<%page args="argument" />
% if argument.type == StringTypes.TEXT:
    "\
% endif
${argument.data}\
% if argument.type == StringTypes.TEXT:
    "\
% endif
