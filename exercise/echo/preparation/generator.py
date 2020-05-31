import json

TESTCASES = 50

contexts = []
for i in range(TESTCASES):
    contexts.append({
        "context_testcase": {
            "output": {
                "stdout": {
                    "data": f"invoertekst-{i}",
                    "type": "text"
                }
            },
            "input":  {
                "stdin":     {
                    "data": f"invoertekst-{i}",
                    "type": "text"
                },
                "main_call": True
            }
        }
    })

plan = {
    "tabs": [{
        "name":     "Feedback",
        "contexts": contexts
    }]
}

f = open("../evaluation/full.tson", "w")

json.dump(plan, f, indent=2)
