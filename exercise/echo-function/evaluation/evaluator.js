function evaluate(actual) {
    const correct = actual === "correct";
    return {
        "result": correct,
        "readable_expected": "correct",
        "readable_actual": actual.toString(),
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

function evaluateValue(expected, actual, args) {
    return {
        "result": expected === actual,
        "expected": expected,
        "actual": actual,
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

exports.evaluate = evaluate;
exports.evaluateValue = evaluateValue;