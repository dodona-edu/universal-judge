function evaluate(actual) {
    const correct = actual === "correct";
    return {
        "result": correct,
        "readable_expected": "correct",
        "readable_actual": actual.toString(),
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

function evaluateValue(context) {
    return {
        "result": context.expected === context.actual,
        "readable_expected": context.expected,
        "readable_actual": context.actual,
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

function evaluateValueDsl(context) {
    return {
        "result": context.expected === context.actual,
        "dsl_expected": "{5, 5}",
        "dsl_actual": "{4, 4}",
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

exports.evaluate = evaluate;
exports.evaluateValue = evaluateValue;
exports.evaluateValueDsl = evaluateValueDsl;
