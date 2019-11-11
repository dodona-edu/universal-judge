<%! from testplan import ValueType %>
<%page args="argument" />
% if argument.type == ValueType.text:
    "\
% endif
${argument.data}\
% if argument.type == ValueType.text:
    "\
% endif
