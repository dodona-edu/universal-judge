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

def is_valid_passport(passport):

    """
    >>> is_valid_passport('ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm')
    True
    >>> is_valid_passport('iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929')
    False
    >>> is_valid_passport('hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm')
    True
    >>> is_valid_passport('hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in')
    False
    """

    # extract the set of fields from the passport
    fields = {field.split(':')[0]:field.split(':')[1] for field in passport.split()}

    # check if all mandatory fields are present
    return set(mandatory_fields) <= set(fields)

def passports(filename):

    """
    >>> list(passports('passports.txt'))
    ['ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm', 'iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929', 'hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm', 'hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in']
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
    >>> count_valid_passports('passports.txt')
    2
    >>> count_valid_passports('adventofcode.input.txt')
    247
    """

    # count the number of valid passports in the given file
    return sum(is_valid_passport(passport) for passport in passports(filename))

if __name__ == '__main__':
    import doctest
    doctest.testmod()
