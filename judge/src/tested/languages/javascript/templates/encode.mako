const values = require('./values.js');
const fs = require("fs");

% for statement in statements:
    values.sendValue(process.stdout.fd, <%include file="statement.mako" args="statement=statement" />);
    fs.writeSync(process.stdout.fd, "\n");
% endfor
