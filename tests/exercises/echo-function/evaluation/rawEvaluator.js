function evaluate(actual) {
    return {
        "result": actual === "correct",
        "readable_expected": "correct",
        "readable_actual": actual,
        "messages": []
    }
}

exports.evaluate = evaluate;
