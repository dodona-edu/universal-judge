// Minimal RPC language in JSON to send data from the tests to the judge."""
const fs = require("fs");

function encode(value) {

    let type;

    if (typeof value === "undefined") {
        type = "unknown";
    } else if (typeof value === "boolean") {
        type = "boolean";
    } else if (typeof value === "number") {
        if (value.isInteger()) {
            type = "integer";
        } else {
            type = "rational";
        }
    } else if (typeof value === "string") {
        type = "text";
    } else if (typeof value === "bigint") {
        type = "bigint";
        value = value.toString();
    } else if (typeof value === "symbol") {
        type = "unknown";
        value = value.toString();
    } else if (typeof value === "object") {
        if (value === null) {
            type = "nothing";
        } else if (Array.isArray(value)) {
            type = "list";
            value = value.map(encode);
        } else if (value instanceof Set) {
            type = "set";
            value = Array.from(value).map(encode);
        } else if (value instanceof Map) {
            type = "set";
            value = Array
                .from(value)
                .reduce(
                    (obj, [key, value]) => (
                        // Be careful! Maps can have non-String keys; object literals can't.
                        Object.assign(obj, { [key]: encode(value) })
                    ),
                    {}
                );
        } else {
            type = "object";
        }
    } else {
        type = "unknown";
    }

    return {
        type: type,
        data: value
    };

}

// Send a value to the given stream.
function sendValue(stream, value) {
    fs.writeSync(stream, JSON.stringify(encode(value)));
}

// Send an exception to the given stream.
function sendException(stream, exception) {
    if (!exception) {
        return;
    }
    fs.writeSync(stream, JSON.stringify({
        "message": exception.toString(),
        "stacktrace": exception.stack ? exception.stack : ""
    }));
}

exports.sendValue = sendValue
exports.sendException = sendException
