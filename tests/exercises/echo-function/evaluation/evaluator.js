function evaluate(actual) {
    const correct = actual === "correct";
    return {
        "result": correct,
        "readable_expected": "correct",
        "readable_actual": actual.toString(),
        "messages": [{"description": "Hallo", "format": "text"}]
    }
}

exports.evaluate = evaluate;
