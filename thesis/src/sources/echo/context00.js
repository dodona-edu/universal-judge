const fs = require('fs');
const vm = require('vm');
const values = require("./values.js");
const valueFile = fs.openSync("TY4l6Xnqk_values.txt", "w");
const exceptionFile = fs.openSync("TY4l6Xnqk_exceptions.txt", "w");
function writeSeparator() {
    fs.writeSync(valueFile, "--TY4l6Xnqk-- SEP");
    fs.writeSync(exceptionFile, "--TY4l6Xnqk-- SEP");
    fs.writeSync(process.stdout.fd, "--TY4l6Xnqk-- SEP");
    fs.writeSync(process.stderr.fd, "--TY4l6Xnqk-- SEP");
}
function sendValue(value) {
    values.sendValue(valueFile, value);
}
function sendException(exception) {
    values.sendException(exceptionFile, exception);
}
function sendSpecificValue(value) {
    values.sendEvaluated(valueFile, value);
}
function sendSpecificException(exception) {
    values.sendEvaluated(exceptionFile, exception);
}
new_args = [process.argv[0]]
new_args = new_args.concat([])
process.argv = new_args
try {
    writeSeparator();
    eval(fs.readFileSync("submission.js") + "");
    sendException(null)
} catch(e) {
    sendException(e)
}
fs.closeSync(valueFile);
fs.closeSync(exceptionFile);