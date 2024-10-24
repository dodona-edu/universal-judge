function evaluate(actual: string) {
    const correct = actual === "correct";
    return {
        "result": correct,
        "readable_expected": "correct",
        "readable_actual": actual.toString(),
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

function evaluateSum(actual: { toString: () => any; }, sum: number) {
    const correct = sum == 10;
    return {
        "result": correct,
        "readable_expected": "correct",
        "readable_actual": actual.toString(),
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

exports.evaluate = evaluate;
exports.evaluateSum = evaluateSum;
