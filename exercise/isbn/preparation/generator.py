import os
import imp
import sys
import random
import string

# set fixed seed
random.seed(123456789)

# helper function for computing the ISBN-10 check digit
def checkdigit10(code):
    
    # compute the check digit
    check = sum((i + 1) * int(code[i]) for i in range(9)) % 11
    
    # convert the check digit into string representation
    return 'X' if check == 10 else str(check)

# helper function for computing the ISBN-10 check digit
def checkdigit13(code):
    
    # compute the check digit
    check = sum((3 if i % 2 else 1) * int(code[i]) for i in range(12))

    # convert the check digit into a single digit
    return str((10 - check) % 10)

def randomCharacters(length, alphabet):
    
    return ''.join(random.choice(alphabet) for _ in range(length))

# load functionality defined in sample solution
solution = imp.load_source(
    'solution', 
    os.path.join('..', 'solution', 'solution.en.py')
)
for name in dir(solution):
    if not (name.startswith('__') and name.endswith('__')):
        globals()[name] = eval('solution.{}'.format(name))

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
    code = randomCharacters(length - 1, string.digits)
    if length == 10:
        if random.random() < 0.5:
            code += checkdigit10(code)
        else:
            code += random.choice(string.digits + 'X')
    else:
        if random.random() < 0.5:
            code += checkdigit13(code)
        else:
            code += random.choice(string.digits)
    args.append((code, random.choice([None, True, False])))    

# generate unit tests for function isISBN
sys.stdout = open(os.path.join('..', 'evaluation', '0.in'), 'w', encoding='utf-8')
for code, isbn13 in args:
    if isbn13 is None:
        print(f'>>> isISBN({code!r})')
    else:
        print(f'>>> isISBN({code!r}, {isbn13})')
    print(isISBN(code, isbn13 if isbn13 is not None else True))
    print()

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
        code = randomCharacters(length - 1, string.digits)
        if length == 10:
            if random.random() < 0.5:
                code += checkdigit10(code)
            else:
                code += random.choice(string.digits + 'X')
        else:
            if random.random() < 0.5:
                code += checkdigit13(code)
            else:
                code += random.choice(string.digits)
        codes.append(code)

    args.append((codes, random.choice([None, True, False])))    

# generate unit tests for function areISBN
sys.stdout = open(os.path.join('..', 'evaluation', '1.in'), 'w', encoding='utf-8')
for index, (codes, isbn13) in enumerate(args):
    index += 1
    print(f'>>> codes{index:02d} = {codes} # doctest: +NEWCONTEXT')
    if isbn13 is None:
        print(f'>>> areISBN(codes{index:02d})')
    else:
        print(f'>>> areISBN(codes{index:02d}, {isbn13})')
    print(areISBN(codes, isbn13))
    print()
