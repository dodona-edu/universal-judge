import random
import string
import json

# De implementatie van het serialisatieformaat.
from isbn.preparation import values
# De voorbeeldoplossing.
from isbn.solution import solution

# Met een vaste seed krijgen we deterministische resultaten.
random.seed(123456789)

# We halen de naam van het testplan op uit de configuratie van de oefening.
with open("../config.json", "r") as config_file:
    config = json.load(config_file)
testplan_name = config["evaluation"].get("plan_name", "plan.json")

# Of alle testgevallen in dezelfde context moeten plaatsvinden of niet.
# Aangezien ze onafhankelijk zijn, doen we dit niet.
# Meerdere contexten hebben echter wel een performantiekost, waardoor we de
# optie toch voorzien.
ONE_CONTEXT = False


def check_digit10(code):
    """Bereken het controlecijfer voor een ISBN van lengte 10."""
    check = sum((i + 1) * int(code[i]) for i in range(9)) % 11
    # Zet het controlecijfer om naar een string.
    return 'X' if check == 10 else str(check)


def check_digit13(code):
    """Bereken het controlecijfer voor een ISBN van lengte 13."""
    check = sum((3 if i % 2 else 1) * int(code[i]) for i in range(12))
    # Zet het controlecijfer om naar een string.
    return str((10 - check) % 10)


def random_characters(length, alphabet):
    """Genereer een aantal willekeurige tekens uit een alfabet."""
    return ''.join(random.choice(alphabet) for _ in range(length))


def generate_code():
    """Genereer een ISBN-code, met zowel lengte 10 als 13."""
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
    return code


def generate_is_isbn():
    """
    Genereer de contexten voor de "is_isbn"-functie.
    :return: De gegenereerde contexten.
    """
    contexts = []

    # Genereer eerst de argumenten voor de functie "is_isbn".
    # We beginnen met wat vaste combinaties.
    args = [
        ('9789027439642', False),
        ('9789027439642', True),
        ('9789027439642', None),
        ('080442957X', None),
        ('080442957X', False),
        (9789027439642, None),
    ]
    # Voor de rest vullen we aan met willekeurige argumenten.
    while len(args) < 50:
        code = generate_code()
        args.append((code, random.choice([None, True, False])))

    # Genereer de eigenlijke contexten.
    for code, isbn13 in args:
        # Eerst doen we de functieoproep. We geven zeker de ISBN mee.
        function_arguments = [
            values.encode(code)
        ]
        # Indien nodig geven we ook het tweede argument mee.
        if isbn13 is not None:
            function_arguments.append(values.encode(isbn13))

        # Bereken het resultaat met de gegeven argumenten.
        result = solution.is_isbn(code, isbn13 if isbn13 is not None else True)

        # Ons testgeval bevat de functieoproep als invoer, en de berekende waarde
        # als verwachte uitvoer.
        testcase = {
            "input":  {
                "expression": {
                    "type":      "function",
                    "name":      "is_isbn",
                    "arguments": function_arguments
                }
            },
            "output": {
                "result": {
                    "value": (values.encode(result))
                }
            }
        }

        # Steek het testgeval in een context.
        context = {
            "testcases": [testcase]
        }
        contexts.append(context)

    return contexts


def generate_are_isbn():
    """
    Genereer de contexten voor de "are_isbn"-functie.
    :return: De gegenereerde contexten.
    """

    # Vaste invoerargumenten die we zeker in het testplan willen.
    codes = [
        '0012345678', '0012345679', '9971502100', '080442957X', 5, True,
        'The Practice of Computing Using Python', '9789027439642', '5486948320146'
    ]
    codes2 = ['012345678' + str(digit) for digit in range(10)]
    args = [
        (codes, None),
        (codes, True),
        (codes, False),
        (codes2, None),
        (codes2, True),
        (codes2, False),
    ]
    # Vul opnieuw de rest aan tot we aan 50 zitten.
    while len(args) < 50:
        codes = [generate_code() for _ in range(random.randint(4, 10))]
        args.append((codes, random.choice([None, True, False])))

    # Genereer de eigenlijke contexten.
    contexts = []
    for index, (codes, isbn13) in enumerate(args):
        index += 1
        # Deze keer pakken we het iets anders aan: de lijst van ISBN's kennen we
        # eerst toe aan een variabele, en geven het niet rechtstreeks mee als
        # argument. Maak het testgeval voor de assignment.
        assignment_testcase = {
            "input": {
                "statement": {
                    "name": f"codes{index:02d}",
                    "expression": values.encode(codes),
                    "type": {
                        "type": "list"
                    }
                }
            }
        }

        # Stel de functieargumenten op. We geven opnieuw sowieso de variabele die
        # we eerst hebben aangemaakt mee als argument.
        function_arguments = [
            f"codes{index:02d}"
        ]

        # Voeg het tweede argument toe indien nodig.
        if isbn13 is not None:
            function_arguments.append(values.encode(isbn13))

        # Bereken het resultaat.
        result = solution.are_isbn(codes, isbn13 if isbn13 is not None else None)

        # Maak het normale testgeval. We hebben opnieuw als invoer de functie en als
        # uitvoer de verwachte waarde.
        testcase = {
            "input":  {
                "expression": {
                    "type":      "function",
                    "name":      "are_isbn",
                    "arguments": function_arguments
                }
            },
            "output": {
                "result": {
                    "value": (values.encode(result))
                }
            }
        }

        # Voeg beide testcases toe aan een context.
        # Elke testcase heeft hoogstens één functieoproep of assignment, maar niet
        # beide. Daarom hebben we nu twee testgevallen. Het eerste testgeval voor
        # de assignment heeft enkel de standaardtests, wat wil zeggen dat er bv.
        # geen uitvoer op stderr mag zijn.
        context = {
            "testcases": [assignment_testcase, testcase]
        }

        contexts.append(context)
    return contexts


def flatten_contexts(contexts):
    """Voeg alle testgevallen samen tot 1 context."""
    testcases = [context["normal"] for context in contexts]
    flat = [item for sublist in testcases for item in sublist]
    new_context = {
        "testcases": flat
    }
    return new_context


# Creëer de contexten.
tab_1_contexts = generate_is_isbn()
tab_2_contexts = generate_are_isbn()

# Creëer het testplan met de twee tabbladen.
plan = {
    "tabs": [
        {
            "name": "is_isbn",
            "contexts": tab_1_contexts
        },
        {
            "name": "are_isbn",
            "contexts": tab_2_contexts
        }
    ]
}

# Indien we alles in één context willen, doen we dat.
if ONE_CONTEXT:
    new_tab1_context = flatten_contexts(plan["tabs"][0]["contexts"])
    plan["tabs"][0]["contexts"] = [new_tab1_context]
    new_tab2_context = flatten_contexts(plan["tabs"][1]["contexts"])
    plan["tabs"][1]["contexts"] = [new_tab2_context]

# Terugvallen op individuele modus is niet nuttig in Python, dus laten we dat niet
# toe. Indien het terugvallen niet nuttig is, is het sneller om het uit te zetten.
plan["configuration"] = {
    "allow_fallback": False
}

# Schrijf het testplan.
with open(f"../evaluation/{testplan_name}", 'w') as fp:
    json.dump(plan, fp, indent=2)
