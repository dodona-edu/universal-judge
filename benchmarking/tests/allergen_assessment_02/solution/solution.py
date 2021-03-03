def read_foods(filename):

    with open(filename, 'r', encoding='utf-8') as lines:
        for food in lines:
            ingredients, allergens = food.rstrip(')\n').split('(')
            ingredients = ingredients.split()
            allergens = allergens[9:].replace(',', '').split()
            yield set(ingredients), set(allergens)

def dangerous_ingredients(filename):

    """
    >>> dangerous_ingredients('foods.txt')
    'mxmxvkd,sqjhc,fvjkl'
    >>> dangerous_ingredients('adventofcode.input.txt')
    'qqskn,ccvnlbp,tcm,jnqcd,qjqb,xjqd,xhzr,cjxv'
    """

    # read foods from file
    foods = list(read_foods(filename))

    # collect foods per allergen
    possible_ingredients = {}
    for ingredients, allergens in foods:
        for allergen in allergens:
            if allergen not in possible_ingredients:
                possible_ingredients[allergen] = []
            possible_ingredients[allergen].append(ingredients)

    # compute intersection of foods per allergen
    for allergen, ingredients in possible_ingredients.items():
        possible_ingredients[allergen] = set.intersection(*ingredients)

    # resolve what ingredients contain what allergens
    matched_ingredients = {}
    while possible_ingredients:
        reduced_allergens = set()
        for allergen, ingredients in possible_ingredients.items():
            reduced_ingredients = ingredients - set(matched_ingredients)
            if len(reduced_ingredients) == 1:
                ingredient = reduced_ingredients.pop()
                matched_ingredients[ingredient] = allergen
                reduced_allergens.add(allergen)
            else:
                possible_ingredients[allergen] = reduced_ingredients
        for allergen in reduced_allergens:
            del possible_ingredients[allergen]

    return ','.join(sorted(
        matched_ingredients,
        key=lambda ingredient: matched_ingredients[ingredient]
    ))

if __name__ == '__main__':
    import doctest
    doctest.testmod()