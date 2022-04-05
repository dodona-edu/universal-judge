import json

TESTCASES = 50

contexts = []
for i in range(TESTCASES):
    contexts.append({
        "testcases": [
            {
                "input": {
                    "variable": "a_list",
                    "type": "sequence",
                    "expression": {
                        "type": "sequence",
                        "data": [
                            {"type": "integer", "data": i - 1},
                            {"type": "integer", "data": i},
                            {"type": "integer", "data": i},
                            {"type": "integer", "data": i + 1}
                        ]
                    }
                }
            },
            {
            "output": {
                "result": {
                    "value": {
                        "data": [
                            {"type": "integer", "data": i - 1},
                            {"type": "integer", "data": i + 1}
                        ],
                        "type": "sequence"
                    }
                }
            },
            "input": {
                "type": "function",
                "name": "remove",
                "arguments": [
                    "a_list",
                    {
                        "data": i,
                        "type": "integer"
                    }]
            }
        }]
    })

plan = {
    "tabs": [{
        "name": "Feedback",
        "runs": [{
            "contexts": contexts
        }]
    }]
}

f = open("../evaluation/full.tson", "w")

json.dump(plan, f, indent=2)
