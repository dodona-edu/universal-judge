## Responsible for generating a function expression to a custom evaluator.
<%page args="evaluator,function" />\
(async () => {
const ${evaluator} = require('./${evaluator}.js');
const values = require('./values.js');

const result = <%include file="function.mako" args="function=function" />;
values.sendEvaluated(process.stdout.fd, await result);
})();