# YAML cheat sheet

## Key-Value pairs

Example key-value pairs in `YAML`:
```yaml
language: "python"
version: "3.8.6"
supported: true
users: 5
```

Translation in `JSON`:
```json
{
  "language": "python",
  "version": "3.8.6",
  "supported": true,
  "users": 5
}
```

## Sequences

- Example sequence of scalars in `YAML`:
```yaml
- "Hoe slim ben jij"
- "Erfgenaam"
- "Spoorhekcodering"
```

Alternative representation:
```yaml
[ "Hoe slim ben jij", "Erfgenaam", "Spoorhekcodering" ]
```

Translation in `JSON`:
```json
[ "Hoe slim ben jij", "Erfgenaam", "Spoorhekcodering" ]
```

- Example sequence of objects in `YAML`:
```yaml
- exercise: "Hoe slim ben jij"
  type: "input-output"
- exercise: "Erfgenaam"
  type: "function"
```

Translation in `JSON`:
```json
[
  {
    "exercise": "Hoe slim ben jij",
    "type": "input-output"
  },
  {
    "exercise": "Erfgenaam",
    "type": "function"
  }
]
```
## Scalars

### Booleans
Boolean notations in `YAML`:
```yaml
true: true
false: false
"yes": yes
"no": no
```

Translation in `JSON`:
```json
{
  "true": true,
  "false": false,
  "yes": true,
  "no": false
}
```

### Numbers
Number notations in `YAML`:
```yaml
natural: 127
integer: -128
float: 2.2521
float_neg: -125.554
octal: 014
hexa: 0xC
canonical: 1.23015e+3
exponential: 12.3015e+02
fixed: 1230.15
```
```yaml
negative infinity: -.inf
not a number: .NaN
```

Translation in `JSON`:
```json
{
  "natural": 127,
  "integer": -128,
  "float": 2.2521,
  "float_neg": -125.554,
  "octal": 12,
  "hexa": 12,
  "canonical": 1230.15,
  "exponential": 1230.15,
  "fixed": 1230.15
}
```
`NaN` and `Infinity` needs `JSON5`:
```json5
{
  "not a number": NaN,
  "negative infinity": -Infinity
}
```

### String notations
Indentation determines scope in `YAML`:
```yaml
notations:
  unquoted: Unquoted string
  quoted: "Quoted string"
  unquoted_multiline:
    This unquoted scalar
    spans many lines.
  quoted_multiline: "So does this
    quoted scalar.\n"
  folded: >
    This folded scalar
    spans "many" lines,
    but result in one line
  unfolded: |
    This flow scalar,
    spans "many" lines,
    and result in a multi-
    line string
```

Translation in `JSON`:
```json
{
  "notations": {
    "unquoted": "Unquoted string",
    "quoted": "Quoted string",
    "quoted_multiline": "So does this quoted scalar.\n",
    "unquoted_multiline": "This unquoted scalar spans many lines.",
    "folded": "This folded scalar spans \"many\" lines, but result in one line\n",
    "unfolded": "This flow scalar,\nspans \"many\" lines,\nand result in a multi-\nline string"
  }
}
```

### Comments
Comments in `YAML` start with a `#`:
```yaml
# This is a one line comment
data: value # Data contains value
```
