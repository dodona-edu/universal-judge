const fileSystem = require('fs');

function isVanillaObject(value: Object) : boolean {
    try {
        return Reflect.getPrototypeOf(value) === null;
    } catch {
        return false;
    }
}

function encode(value: Object): { data: Object; diagnostic: any; type: string } {
    let diagnostic = null;
    let type = null;

    if ( typeof value === "undefined") {
        type = "undefined";
    } else if (typeof value === "boolean") {
        type = "boolean";
    } else if (typeof value === "number") {
        if (Number.isInteger(value)) {
            type = "integer";
        } else {
            type = "real";
            if (Number.isNaN(value)) {
                value = "nan";
            } else if (!Number.isFinite(value)) {
                if (value < 0) {
                    value = "-inf";
                } else {
                    value = "inf";
                }
            }
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
            type = "null";
        } else if (Array.isArray(value)) {
            type = "list";
            // Handle holes in arrays...
            const unholed = [];
            for (let i = 0; i < value.length; i++) {
                if (!value.hasOwnProperty(i)) {
                    unholed.push(`<empty at index ${i}>`)
                } else {
                    unholed.push(value[i]);
                }
            }
            value = unholed.map(encode);
        } else if (value instanceof Set) {
            type = "set";
            value = Array.from(value).map(encode);
        } else if (value instanceof Map) {
            type = "dictionary";
            value = Array
                    .from(value)
                    .map(([key, value]) => {
                                return {
                                    key: encode(key),
                                    value: encode(value)
                                };
                            }
                    );
        } else if (value?.constructor === Object || isVanillaObject(value)) {
            // Plain objects
            type = "object";
            // Process the elements of the object.

            value = Object.keys(value).map(key => {
                return {
                    key: encode(key),
                    value: encode((value as Record<string, any>)[key])
                };
            });
        } else {
            type = "unknown";
            diagnostic = value?.constructor?.name;
            value = JSON.stringify(value);
        }
    } else {
        type = "unknown";
        diagnostic = (value as Object)?.constructor?.name;
        value = Object.prototype.toString.call(value);
    }

    return {
        type: type,
        data: value,
        diagnostic: diagnostic
    };

}

// Send a value to the given stream.
function sendValue(stream: number, value: Object) {
    fileSystem.writeSync(stream, JSON.stringify(encode(value)));
}

// Send an exception to the given stream.
function sendException(stream: number, exception: Error | Object | {constructor: {name: any}}): void {
    if (!exception) {
        return;
    }
    if (exception instanceof Error) {
        // We have a proper error...
        fileSystem.writeSync(stream, JSON.stringify({
            "message": exception.message,
            "stacktrace": exception.stack ?? "",
            "type": exception.constructor.name
        }));
    } else {
        // Comes out of the values.js:
        // Temporarily allow objects with "message" and "name".
        // TODO: remove this once the semester is over
        // noinspection PointlessBooleanExpressionJS
        if (typeof exception === 'object') {
            fileSystem.writeSync(stream, JSON.stringify({
                "message": (exception as Error).message ?? "",
                "stacktrace": "",
                "type": (exception as Error).name ?? ""
            }));
        } else {
            // We have something else, so we cannot rely on stuff being present.
            fileSystem.writeSync(stream, JSON.stringify({
                "message": JSON.stringify(exception),
                "stacktrace": "",
                "type": (exception as Object).constructor.name ?? (Object.prototype.toString.call(exception)),
                "additional_message_keys": ["languages.javascript.runtime.invalid.exception"]
            }));
        }
    }
}

// Send an evaluation result to the given stream.
function sendEvaluated(stream: number, result: Object) {
    fileSystem.writeSync(stream, JSON.stringify(result));
}

exports.sendValue = sendValue;
exports.sendException = sendException;
exports.sendEvaluated = sendEvaluated;
