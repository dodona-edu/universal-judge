import json

from random import randint

from correct import lottery


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
        s = lottery(a, m)
        return s, a, m


if __name__ == '__main__':
    contexts = []
    tuples = [generate_data() for _ in range(45)]
    for data in tuples:
        print(data)
        contexts.append({"additional": [{
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
            },
            "function": {
                "type": "top",
                "name": "lottery",
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
        }]})

    plan = {
        "tabs": [
            {
                "name": "Correctheid",
                "contexts": contexts
            }
        ]
    }

    with open("plan.json", "w") as f:
        json.dump(plan, f)