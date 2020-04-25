import json

TESTCASES = 50

contexts = []
for i in range(TESTCASES):
    contexts.append({
        "testcases": [{
            "output": {
                "result": {
                    "value": {
                        "type": "text",
                        "data": f"invoertekst-{i}"
                    }
                }
            },
            "input":  {
                "type":      "function",
                "name":      "echo",
                "arguments": [{
                    "data": f"invoertekst-{i}",
                    "type": "text"
                }]
            }
        }]
    })

plan = {
    "tabs": [{
        "name":     "Feedback",
        "contexts": contexts
    }]
}

f = open("../evaluation/full.tson", "w")

json.dump(plan, f, indent=2)
