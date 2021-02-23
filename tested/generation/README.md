# Definition `types.json`

The file `types.json` is used for creating instance of a template description for a specific programming language.

This file contains 4 parts: _brackets_, _console_, _appendix_ and _datatype names_.

```json
{
    "brackets": {},
    "console": {},
    "appendix": "",
    ...
}
```

## Datatype names

For each TESTed datatype supported by your programming language you should define the type name at the root level, this
name must be a string for single datatypes.

For collection datatypes this can be a name or a boolean, which than specifies how the bracket must be place for the
collection type.

- `false` means the brackets should be after the inner datatype
- `true` means the brackets should be around the inner datatypes

When the inner datatypes must have other names then when used as a single datatype, then you must define these names in
the `inner` object from the json
(not when brackets are placed after the datatype).

Example java:

```json
{
    "integer": "int",
    "inner": {
        "integer": "Integer"
    }
}
```

## Brackets

The part hooks defines the brackets used for type definitions like sequence and map.

### Most basic

```json
{
    "brackets": {
        "open": "[",
        "close": "]"
    }
}
```

- `"open"`: Opening bracket
- `"close"`: Opening bracket

Example result:
`sequence[integer]`

### Specific types brackets after inner type or around inner types:

```json
{
    "brackets": {
        "open": "[",
        "close": "]",
        "array": {
            "open": "[",
            "close": "]"
        },
        "tuple": {
            "open": "(",
            "close": ")"
        }
    }
}
```

When using a boolean for a datatype name, you must add for each datatype the corresponding brackets that should be used.

Example after type:
`integer[]`

Example around inner types:
`(text, integer)`

## Console

Information needed for properly generating example statements and expressions are defined in the object `console`. This
object has two keys:

- _name_: the name of the programming language used in Pygments
- _prompt_: the prompt prefix used for an input statement

Example Java:

```json
{
    "console": {
        "name": "java",
        "prompt": ">"
    }
}
```
