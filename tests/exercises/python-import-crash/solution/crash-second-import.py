import builtins

if getattr(builtins, "already_imported", False):
    print(1 / 0)
builtins.already_imported = True
