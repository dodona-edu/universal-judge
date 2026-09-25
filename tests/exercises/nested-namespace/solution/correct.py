from dataclasses import dataclass


@dataclass(frozen=True)
class Grocery:
    item: str
    cost: int
    on_sale: bool


class Basket:

    def __init__(self, groceries):
        self.groceries = groceries


MILK = Grocery("Milk", 3, True)


def sum_groceries(groceries) -> int:
    if isinstance(groceries, dict):
        groceries = groceries.values()
    return sum(grocery.cost for grocery in groceries if grocery.on_sale)


def basket_total(basket: Basket) -> int:
    return sum_groceries(basket.groceries)
