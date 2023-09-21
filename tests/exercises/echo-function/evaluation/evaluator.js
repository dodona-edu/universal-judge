function evaluate(actual) {
    const correct = actual === "correct";
    return {
        "result": correct,
        "readable_expected": "correct",
        "readable_actual": actual.toString(),
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

function evaluateValue(expected, actual) {
    return {
        "result": expected === actual,
        "readable_expected": expected,
        "readable_actual": actual,
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

function evaluateValueDsl(expected, actual) {
    return {
        "result": expected === actual,
        "dsl_expected": "{5, 5}",
        "dsl_actual": "{4, 4}",
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

exports.evaluate = evaluate;
exports.evaluateValue = evaluateValue;
exports.evaluateValueDsl = evaluateValueDsl;
