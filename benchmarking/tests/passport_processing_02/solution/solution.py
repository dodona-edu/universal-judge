import re

# define the set of mandatory fields
mandatory_fields = {
    'byr': [(re.compile('^([0-9]{4})$'), 1920, 2002)],
    'iyr': [(re.compile('^([0-9]{4})$'), 2010, 2020)],
    'eyr': [(re.compile('^([0-9]{4})$'), 2020, 2030)],
    'hgt': [(re.compile('^([0-9]+)cm$'), 150, 193), (re.compile('^([0-9]+)in$'), 59, 78)],
    'hcl': [(re.compile('^#[0-9a-f]{6}$'), None, None)],
    'ecl': [(re.compile('^amb|blu|brn|gry|grn|hzl|oth$'), None, None)],
    'pid': [(re.compile('^[0-9]{9}$'), None, None)],
}

def is_valid_field(field, value):

    """
    >>> is_valid_field('byr', '2002')
    True
    >>> is_valid_field('byr', '2003')
    False

    >>> is_valid_field('hgt', '60in')
    True
    >>> is_valid_field('hgt', '190cm')
    True
    >>> is_valid_field('hgt', '190in')
    False
    >>> is_valid_field('hgt', '190')
    False

    >>> is_valid_field('hcl', '#123abc')
    True
    >>> is_valid_field('hcl', '#123abz')
    False
    >>> is_valid_field('hcl', '123abc')
    False

    >>> is_valid_field('ecl', 'brn')
    True
    >>> is_valid_field('ecl', 'wat')
    False

    >>> is_valid_field('pid', '000000001')
    True
    >>> is_valid_field('pid', '0123456789')
    False
    """

    if field not in mandatory_fields:
        return False

    for regex, lower, upper in mandatory_fields[field]:
        result = regex.match(value)
        if result:

            # match without number is correct
            groups = result.groups()
            if not groups:
                return True

            # check if number is between limits
            value = groups[0]
            if value.isdigit() and lower <= int(value) <= upper:
                return True

    # no match with any of the rules for the field
    return False

def is_valid_passport(passport):

    """
    >>> is_valid_passport('eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926')
    False
    >>> is_valid_passport('iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946')
    False
    >>> is_valid_passport('hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277')
    False
    >>> is_valid_passport('hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007')
    False

    >>> is_valid_passport('pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f')
    True
    >>> is_valid_passport('eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm')
    True
    >>> is_valid_passport('hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022')
    True
    >>> is_valid_passport('iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719')
    True
    """

    # extract the set of fields from the passport
    fields = {field.split(':')[0]:field.split(':')[1] for field in passport.split()}

    # check if all mandatory fields are present
    if not set(mandatory_fields) <= set(fields):
        return False

    # check if all mandatory fields are valid
    return all(
        is_valid_field(field, value)
        for field, value in fields.items() if field in mandatory_fields
    )

def passports(filename):

    """
    >>> list(passports('invalid_passports.txt'))
    ['eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926', 'iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946', 'hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277', 'hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007']
    """

    with open(filename) as lines:
        passport = []
        for line in lines:
            line = line.rstrip('\n')
            if line:
                passport.append(line)
            elif passport:
                yield ' '.join(passport)
                passport = []

        if passport:
            yield ' '.join(passport)

def count_valid_passports(filename):

    """
    >>> count_valid_passports('invalid_passports.txt')
    0
    >>> count_valid_passports('valid_passports.txt')
    4
    >>> count_valid_passports('adventofcode.input.txt')
    145
    """

    # count the number of valid passports in the given file
    return sum(is_valid_passport(passport) for passport in passports(filename))

if __name__ == '__main__':
    import doctest
    doctest.testmod()
