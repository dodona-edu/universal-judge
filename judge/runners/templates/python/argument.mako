<%! from testplan import ValueType %>
<%page args="argument" />
% if argument.name:
    ${argument.name}=\
% endif
% if argument.type == ValueType.text:
    "\
% endif
${argument.data}\
% if argument.type == ValueType.text:
    "\
% endif
