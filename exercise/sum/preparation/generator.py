import json
import random
import string

# Met een vaste seed krijgen we deterministische resultaten.
random.seed(123456789)


def generate_data():
    data = []
    for _ in range(45):
        number_of_numbers = random.randint(0, 10)
        data.append(random.sample(range(-100, 100), number_of_numbers))
    other_data = [*string.ascii_letters, "Hallo World", "twintig", "26.93", "5", "2"]
    for _ in range(5):
        number_of_numbers = random.randint(0, 10)
        data.append(random.sample(other_data, number_of_numbers))
    random.shuffle(data)
    return data


def is_all_int(data):
    for x in data:
        try:
            int(x)
        except ValueError:
            return False
    return True


if __name__ == '__main__':
    contexts = []
    numbers = generate_data()
    for inputs in numbers:
        input_ = {
            "arguments": [str(x) for x in inputs],
            "main_call": True
        }
        if is_all_int(inputs):
            output_ = {
                "stdout": {
                    "type": "text",
                    "data": str(sum(inputs))
                }
            }
        else:
            output_ = {
                "stderr": {
                    "type": "text",
                    "data": "som: ongeldige argumenten"
                },
                "exit_code": {
                    "value": 1
                }
            }
        contexts.append({
            "context_testcase": {
                "input":  input_,
                "output": output_
            }
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
