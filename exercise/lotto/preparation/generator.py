import json
import random

from lotto.solution import correct

# Met een vaste seed krijgen we deterministische resultaten.
random.seed(123456789)


def generate_data():
    for _ in range(45):

        case = random.randint(0, 3)
        count = random.randint(1, 20)
        maximum = random.randint(max(6, count), 10 * count)

        a = 6
        m = 42

        if case == 0:
            pass
        elif case == 1:
            a = count
        elif case == 2:
            m = maximum
        elif case == 3:
            a = count
            m = maximum
        s = correct.loterij(a, m)
        return s, a, m


if __name__ == '__main__':
    contexts = []
    tuples = [generate_data() for _ in range(45)]
    for data in tuples:
        contexts.append({
            "testcases": [{
                "output": {
                    "result": {
                        "value":     {
                            "type": "text",
                            "data": data[0]
                        },
                        "evaluator": {
                            "type":      "programmed",
                            "language":  "python",
                            "path":      "./evaluator.py",
                            "arguments": [
                                {
                                    "type": "integer",
                                    "data": data[1]
                                },
                                {
                                    "type": "integer",
                                    "data": data[2]
                                }
                            ]
                        }
                    }
                },
                "input":  {
                    "type":      "function",
                    "name":      "loterij",
                    "arguments": [
                        {
                            "type": "integer",
                            "data": data[1]
                        },
                        {
                            "type": "integer",
                            "data": data[2]
                        }
                    ]
                }
            }]
        })

    plan = {
        "tabs": [
            {
                "name":     "Feedback",
                "contexts": contexts
            }
        ]
    }

    with open("../evaluation/plan.tson", "w") as f:
        json.dump(plan, f, indent=2)
