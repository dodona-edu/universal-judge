def read_foods(filename):

    with open(filename, 'r', encoding='utf-8') as lines:
        for food in lines:
            ingredients, allergens = food.rstrip(')\n').split('(')
            ingredients = ingredients.split()
            allergens = allergens[9:].replace(',', '').split()
            yield set(ingredients), set(allergens)

def inert_ingredients(filename):

    """
    >>> inert_ingredients('foods.txt')
    5
    >>> inert_ingredients('adventofcode.input.txt')
    2230
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

    # compute union of all remaining ingredients
    possible_ingredients = set.union(*possible_ingredients.values())

    return sum(len(ingredients - possible_ingredients) for ingredients, _ in foods)

if __name__ == '__main__':
    import doctest
    doctest.testmod()