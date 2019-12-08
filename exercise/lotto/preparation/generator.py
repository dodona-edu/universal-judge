import json

from random import randint

import sys
sys.path.append("../solution/")

try:
    from ..solution.correct import loterij
except:
    # noinspection PyUnresolvedReferences
    from correct import loterij


def generate_data():
    for _ in range(45):

        case = randint(0, 3)
        count = randint(1, 20)
        maximum = randint(max(6, count), 10 * count)

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
        s = loterij(a, m)
        return s, a, m


if __name__ == '__main__':
    contexts = []
    tuples = [generate_data() for _ in range(45)]
    for data in tuples:
        print(data)
        contexts.append({"normal": [{
            "output": {
                "result": {
                    "value": {
                        "type": "list",
                        "data": [
                            {
                                "type": "text",
                                "data": data[0]
                            },
                            {
                                "type": "integer",
                                "data": data[1]
                            },
                            {
                                "type": "integer",
                                "data": data[2]
                            }
                        ]
                    },
                    "evaluator": {
                        "type": "custom",
                        "language": "python",
                        "code": {
                            "type": "file",
                            "data": "./evaluator.py"
                        }
                    }
                }
            },
            "input": {
                "function": {
                    "type": "top",
                    "name": "loterij",
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
        }]})

    plan = {
        "tabs": [
            {
                "name": "Feedback",
                "contexts": contexts
            }
        ]
    }

    with open("../evaluation/plan.json", "w") as f:
        json.dump(plan, f)
