"""
Generate a testplan for the exercise.
"""
import random
import string
import json

# set fixed seed
random.seed(123456789)

# Find the name of the testplan.
with open("../config.json", "r") as config_file:
    config = json.load(config_file)

testplan_name = config["evaluation"].get("plan_name", "plan.json")

# If all tests should go into one context?
ONE_CONTEXT = False


def _encode(value):
    """TODO: this is copied from the judge, provide a library?"""
    if value is None:
        type_ = "nothing"
        data_ = value
    elif isinstance(value, str):
        type_ = "text"
        data_ = value
    elif isinstance(value, bool):
        type_ = "boolean"
        data_ = value
    elif isinstance(value, int):
        type_ = "integer"
        data_ = value
    elif isinstance(value, float):
        type_ = "rational"
        data_ = value
    elif isinstance(value, list) or isinstance(value, tuple):
        type_ = "list"
        data_ = [_encode(x) for x in value]
    elif isinstance(value, set):
        type_ = "set"
        data_ = [_encode(x) for x in value]
    elif isinstance(value, dict):
        type_ = "object"
        data_ = {str(k): _encode(v) for k, v in value.items()}
    else:
        type_ = "unknown"
        data_ = str(value)

    return {
        "data": data_,
        "type": type_
    }

def check_digit10(code):
    """helper function for computing the ISBN-10 check digit"""
    
    # compute the check digit
    check = sum((i + 1) * int(code[i]) for i in range(9)) % 11
    
    # convert the check digit into string representation
    return 'X' if check == 10 else str(check)


def check_digit13(code):
    """helper function for computing the ISBN-13 check digit"""
    
    # compute the check digit
    check = sum((3 if i % 2 else 1) * int(code[i]) for i in range(12))

    # convert the check digit into a single digit
    return str((10 - check) % 10)


def random_characters(length, alphabet):
    return ''.join(random.choice(alphabet) for _ in range(length))


# load functionality defined in sample solution
try:
    from ..solution import solution
except:
    import solution

contexts = []
is_isbn = {
    "name": "is_isbn",
    "contexts": contexts
}
plan = {
    "tabs": [is_isbn]
}

# generate test cases for function isISBN
args = [
    ('9789027439642', False),
    ('9789027439642', True),
    ('9789027439642', None),
    ('080442957X', None),
    ('080442957X', False),
    (9789027439642, None),
]
while len(args) < 50:
    length = random.choice([10, 13])
    code = random_characters(length - 1, string.digits)
    if length == 10:
        if random.random() < 0.5:
            code += check_digit10(code)
        else:
            code += random.choice(string.digits + 'X')
    else:
        if random.random() < 0.5:
            code += check_digit13(code)
        else:
            code += random.choice(string.digits)
    args.append((code, random.choice([None, True, False])))

# generate unit tests for function isISBN
for code, isbn13 in args:
    function_arguments = [
        _encode(code)
    ]
    if isbn13 is not None:
        function_arguments.append(_encode(isbn13))

    isbn13_param = isbn13 if isbn13 is not None else True

    output_ = {
        "result": {
            "value": (_encode(solution.is_isbn(code, isbn13_param)))
        }
    }

    testcase = {
        "input":  {
            "function": {
                "type":      "top",
                "name":      "is_isbn",
                "arguments": function_arguments
            }
        },
        "output": output_
    }

    context = {
        "normal": [testcase]
    }

    contexts.append(context)

# generate test cases for function areISBN
codes = ['0012345678', '0012345679', '9971502100', '080442957X', 5, True, 'The Practice of Computing Using Python', '9789027439642', '5486948320146']
codes2 = ['012345678' + str(digit) for digit in range(10)]
args = [
    (codes, None),
    (codes, True),
    (codes, False),
    (codes2, None),
    (codes2, True),
    (codes2, False),
]
while len(args) < 50:
    
    codes = []
    
    for _ in range(random.randint(4, 10)):
     
        length = random.choice([10, 13])
        code = random_characters(length - 1, string.digits)
        if length == 10:
            if random.random() < 0.5:
                code += check_digit10(code)
            else:
                code += random.choice(string.digits + 'X')
        else:
            if random.random() < 0.5:
                code += check_digit13(code)
            else:
                code += random.choice(string.digits)
        codes.append(code)

    args.append((codes, random.choice([None, True, False])))    

# generate unit tests for function areISBN
contexts = []
are_isbn = {
    "name": "are_isbn",
    "contexts": contexts
}
plan["tabs"].append(are_isbn)
for index, (codes, isbn13) in enumerate(args):
    index += 1
    assignment_testcase = {
        "input": {
            "assignment": {
                "name": f"codes{index:02d}",
                "expression": {
                    "type": "identity",
                    "arguments": [
                        _encode(codes)
                    ]
                }
            }
        }
    }

    function_arguments = [
        {
            "type": "literal",
            "data": f"codes{index:02d}"
        }
    ]

    if isbn13 is not None:
        function_arguments.append(_encode(isbn13))

    isbn13_param = isbn13 if isbn13 is not None else None

    output_ = {
        "result": {
            "value": (_encode(solution.are_isbn(codes, isbn13_param)))
        }
    }

    testcase = {
        "input":  {
            "function": {
                "type":      "top",
                "name":      "are_isbn",
                "arguments": function_arguments
            }
        },
        "output": output_
    }

    context = {
        "normal": [assignment_testcase, testcase]
    }

    contexts.append(context)


def flatten_contexts(contexts):
    testcases = [context["normal"] for context in contexts]
    flat = [item for sublist in testcases for item in sublist]
    new_context = {
        "normal": flat
    }
    return new_context


if ONE_CONTEXT:
    new_tab1_context = flatten_contexts(plan["tabs"][0]["contexts"])
    plan["tabs"][0]["contexts"] = [new_tab1_context]
    new_tab2_context = flatten_contexts(plan["tabs"][1]["contexts"])
    plan["tabs"][1]["contexts"] = [new_tab2_context]


with open(f"../evaluation/{testplan_name}", 'w') as fp:
    json.dump(plan, fp, indent=2)
