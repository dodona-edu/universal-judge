// Minimal RPC language in JSON to send data from the tests to the judge."""
const fs = require("fs");

function encode(value) {

    let type;

    if (typeof value === "undefined") {
        type = "undefined";
    } else if (typeof value === "boolean") {
        type = "boolean";
    } else if (typeof value === "number") {
        if (Number.isInteger(value)) {
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
            type = "map";
            value = Array
                    .from(value)
                    .map(([key, value]) => {
                                return {
                                    key: encode(key),
                                    value: encode(value)
                                };
                            }
                    );
        } else {
            type = "map";
            // Process the elements of the object.
            value = Object.entries(value).map(([key, value]) => {
                return {
                    key: encode(key),
                    value: encode(value)
                };
            });
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
        "message": exception.message,
        "stacktrace": exception.stack ? exception.stack : ""
    }));
}

// Send an evaluation result to the given stream.
function sendEvaluated(stream, result) {
    fs.writeSync(stream, JSON.stringify(result));
}

exports.sendValue = sendValue;
exports.sendException = sendException;
exports.sendEvaluated = sendEvaluated;
