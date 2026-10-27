class Item:
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"Item({self.value})"


def make_item(value):
    return Item(value)


def make_items(value):
    return [Item(value), Item(value + 1)]


def make_item_dict(value):
    return {"item": Item(value)}
