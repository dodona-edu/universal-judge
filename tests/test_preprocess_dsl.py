from pathlib import Path

import yaml
from pytest_mock import MockerFixture

import tested
from tested.dodona import ExtendedMessage, Permission
from tested.dsl.translate_parser import (
    ExpressionString,
    ReturnOracle,
    _parse_yaml,
    _validate_dsl,
)
from tested.nat_translation import (
    convert_to_yaml,
    create_enviroment,
    parse_yaml,
    translate_file,
    translate_yaml,
    validate_pre_dsl,
)
from tested.transform_json import transform_ide, transform_json, transform_monolingual


def validate_natural_translate(yaml_str: str, translated_yaml_str: str):
    enviroment = create_enviroment()
    yaml_object = parse_yaml(yaml_str)
    translated_dsl = translate_yaml(yaml_object, {}, "en", enviroment)
    translated_yaml = convert_to_yaml(translated_dsl)
    assert translated_yaml.strip() == translated_yaml_str


def test_files_and_descriptions():
    yaml_str = """
translations:
  animal:
    en: "animals"
    nl: "dieren"
  result:
    en: "results"
    nl: "resultaten"
tabs:
  - tab: "{{animal}}"
    contexts:
      - testcases:
        - statement: !natural_language
            en: '{{result}}: dict = Trying("file.txt")'
            nl: '{{result}}: dict = Proberen("fileNL.txt")'
        - expression: !natural_language
            en: 'count_words({{result}})'
            nl: 'tel_woorden({{result}})'
          return: !natural_language
            en: 'The {{result}} is 10'
            nl: 'Het {{result}} is 10'
          description: !natural_language
            en: "Ten"
            nl: "Tien"
        files: !natural_language
          en:
            - name: "file.txt"
              url: "media/workdir/file.txt"
          nl:
            - name: "fileNL.txt"
              url: "media/workdir/fileNL.txt"
        translations:
          result:
            en: "results_context"
            nl: "resultaten_context"
    """.strip()
    translated_yaml_str = """
tabs:
- tab: animals
  contexts:
  - testcases:
    - statement: 'results_context: dict = Trying("file.txt")'
    - expression: count_words(results_context)
      return: The results_context is 10
      description: Ten
    files:
    - name: file.txt
      url: media/workdir/file.txt
""".strip()
    validate_natural_translate(yaml_str, translated_yaml_str)


def test_return():
    yaml_str = """
translations:
  animal:
    en: "animals"
    nl: "dieren"
  result:
    en: "results"
    nl: "resultaten"
tabs:
  - tab: "{{ animal|braces }}_{{ '{' + result + '}' }}"
    translations:
      animal:
        en: "animal_tab"
        nl: "dier_tab"
    contexts:
      - testcases:
        - expression: !natural_language
            en: "count"
            nl: "tellen"
          return: !natural_language
            en: !expression 'count'
            nl: !expression 'tellen'
        - expression: 'ok(10)'
          return: !oracle
            value: !natural_language
              en: "The {{result}} 10 is OK!"
              nl: "Het {{result}} 10 is OK!"
            oracle: "custom_check"
            file: "test.py"
            name: "evaluate_test"
            arguments: !natural_language
              en: ["The value", "is OK!", "is not OK!"]
              nl: ["Het {{result}}", "is OK!", "is niet OK!"]
    """.strip()
    translated_yaml_str = """
tabs:
- tab: '{animal_tab}_{results}'
  contexts:
  - testcases:
    - expression: count
      return: !expression 'count'
    - expression: ok(10)
      return: !oracle
        value: The results 10 is OK!
        oracle: custom_check
        file: test.py
        name: evaluate_test
        arguments:
        - The value
        - is OK!
        - is not OK!
""".strip()
    validate_natural_translate(yaml_str, translated_yaml_str)


def test_nat_lang_and_prog_lang_combination():
    yaml_str = """
translations:
  animal:
    en: "animals"
    nl: "dieren"
tabs:
  - tab: '{{animal}}'
    testcases:
      - expression: !natural_language
          en: "tests(11)"
          nl: "testen(11)"
        return: 11
      - expression: !programming_language
          javascript: "{{animal}}_javascript(1 + 1)"
          typescript: "{{animal}}_typescript(1 + 1)"
          java: "Submission.{{animal}}_java(1 + 1)"
          python: !natural_language
            en: "{{animal}}_python_en(1 + 1)"
            nl: "{{animal}}_python_nl(1 + 1)"
        return: 2
""".strip()
    translated_yaml_str = """
tabs:
- tab: animals
  testcases:
  - expression: tests(11)
    return: 11
  - expression: !programming_language
      javascript: animals_javascript(1 + 1)
      typescript: animals_typescript(1 + 1)
      java: Submission.animals_java(1 + 1)
      python: animals_python_en(1 + 1)
    return: 2
""".strip()
    validate_natural_translate(yaml_str, translated_yaml_str)


def test_format_expression():
    yaml_str = """
translations:
  select:
    en: "select"
    nl: "selecteer"
tabs:
  - tab: 'test'
    testcases:
      - expression: "{{select}}('a', {'a': 1, 'b': 2})"
        return: 1
""".strip()
    translated_yaml_str = """
tabs:
- tab: test
  testcases:
  - expression: 'select(''a'', {''a'': 1, ''b'': 2})'
    return: 1
""".strip()
    validate_natural_translate(yaml_str, translated_yaml_str)


def test_natural_translate_context():
    yaml_str = """
translations:
  result:
    en: "results"
    nl: "resultaten"
tabs:
  - tab: "task"
    contexts: !natural_language
        en:
          - testcases:
            - statement: '{{result}} = Trying(10)'
            - expression: 'count_words({{result}})'
              return: 'The {{result}} is 10'
            - expression: "count"
              return: !expression 'count'
        nl:
          - testcases:
            - statement: '{{result}} = Proberen(10)'
            - expression: 'tel_woorden({{result}})'
              return: 'Het {{result}} is 10'
            - expression: "tellen"
              return: !expression 'tellen'
""".strip()
    translated_yaml_str = """
tabs:
- tab: task
  contexts:
  - testcases:
    - statement: results = Trying(10)
    - expression: count_words(results)
      return: The results is 10
    - expression: count
      return: !expression 'count'
""".strip()
    validate_natural_translate(yaml_str, translated_yaml_str)


def test_natural_translate_testcases_in_context():
    yaml_str = """
translations:
  result:
    en: "results"
    nl: "resultaten"
tabs:
  - tab: "task2"
    contexts:
        - testcases: !natural_language
            en:
              - statement: '{{result}} = Trying(10)'
              - expression: 'count_words({{result}})'
                return: 'The {{result}} is 10'
              - expression: "count"
                return: !expression 'count'
            nl:
              - statement: '{{result}} = Proberen(10)'
              - expression: 'tel_woorden({{result}})'
                return: 'Het {{result}} is 10'
              - expression: "tellen"
                return: !expression 'tellen'
""".strip()
    translated_yaml_str = """
tabs:
- tab: task2
  contexts:
  - testcases:
    - statement: results = Trying(10)
    - expression: count_words(results)
      return: The results is 10
    - expression: count
      return: !expression 'count'
""".strip()
    validate_natural_translate(yaml_str, translated_yaml_str)


def test_natural_translate_testcases():
    yaml_str = """
translations:
  result:
    en: "results"
    nl: "resultaten"
tabs:
  - tab: "task3"
    testcases: !natural_language
        en:
          - statement: '{{result}} = Trying(10)'
          - expression: 'count_words({{result}})'
            return: 'The {{result}} is 10'
          - expression: "count"
            return: !expression 'count'
        nl:
          - statement: '{{result}} = Proberen(10)'
          - expression: 'tel_woorden({{result}})'
            return: 'Het {{result}} is 10'
          - expression: "tellen"
            return: !expression 'tellen'
""".strip()
    translated_yaml_str = """
tabs:
- tab: task3
  testcases:
  - statement: results = Trying(10)
  - expression: count_words(results)
    return: The results is 10
  - expression: count
    return: !expression 'count'
""".strip()
    validate_natural_translate(yaml_str, translated_yaml_str)


def test_natural_translate_io_test():
    yaml_str = """
units:
  - unit: !natural_language
      en: "Arguments"
      nl: "Argumenten"
    translations:
      User:
        en: "user"
        nl: "gebruiker"
    cases:
      - script:
        - stdin: !natural_language
            en: "User_{{User}}"
            nl: "Gebruiker_{{User}}"
          arguments: !natural_language
            en: [ "input_{{User}}", "output_{{User}}" ]
            nl: [ "invoer_{{User}}", "uitvoer_{{User}}" ]
          stdout: !natural_language
            en: "Hi {{User}}"
            nl: "Hallo {{User}}"
          stderr: !natural_language
            en: "Nothing to see here {{User}}"
            nl: "Hier is niets te zien {{User}}"
          exception: !natural_language
            en: "Does not look good"
            nl: "Ziet er niet goed uit"
""".strip()
    translated_yaml_str = """
units:
- unit: Arguments
  cases:
  - script:
    - stdin: User_user
      arguments:
      - input_user
      - output_user
      stdout: Hi user
      stderr: Nothing to see here user
      exception: Does not look good
""".strip()
    validate_natural_translate(yaml_str, translated_yaml_str)


def test_validation():
    yaml_str = """
translations:
  animal:
    en: "animals"
    nl: "dieren"
  result:
    en: "results"
    nl: "resultaten"
  elf:
    en: "eleven"
    nl: "elf"
  select:
    en: "select"
    nl: "selecteer"
tabs:
  - tab: "{{animal}}"
    contexts:
      - testcases:
        - expression: !natural_language
            en: "count"
            nl: "tellen"
          return: !natural_language
            en: !expression 'count'
            nl: !expression 'tellen'
        - expression: 'ok(10)'
          return: !oracle
            value: !natural_language
              en: "The {{result}} 10 is OK!"
              nl: "Het {{result}} 10 is OK!"
            oracle: "custom_check"
            file: "test.py"
            name: "evaluate_test"
            arguments: !natural_language
              en: ["The value", "is OK!", "is not OK!"]
              nl: ["Het {{result}}", "is OK!", "is niet OK!"]
        """
    yaml_object = parse_yaml(yaml_str)
    validate_pre_dsl(yaml_object)

    enviroment = create_enviroment()
    translated_data = translate_yaml(yaml_object, {}, "en", enviroment)
    translated_yaml_string = convert_to_yaml(translated_data)
    _validate_dsl(_parse_yaml(translated_yaml_string))


def test_wrong_natural_translation_suite():
    yaml_str = """
tabs:
- tab: animals
  testcases:
  - expression: tests(11)
    return: !exp 11
  - expression:
      javascript: animals_javascript(1 + 1)
      typescript: animals_typescript(1 + 1)
      java: Submission.animals_java(1 + 1)
      python:
        en: animals_python_en(1 + 1)
        nl: animals_python_nl(1 + 1)
    return: 2
    """.strip()
    parsed_yaml = parse_yaml(yaml_str)
    try:
        validate_pre_dsl(parsed_yaml)
    except ExceptionGroup:
        print("As expected")
    else:
        assert False, "Expected ExceptionGroup error"


def test_run_is_correct(mocker: MockerFixture):
    s = mocker.spy(tested.nat_translation, name="generate_new_yaml")  # type: ignore[reportAttributeAccessIssue]
    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in [
            """
tabs:
- tab: task3
  testcases:
  - statement: !natural_language
        nl: resultaten = Proberen(10)
        en: results = Tries(10)
  - expression: !natural_language
        nl: tel_woorden(resultaten)
        en: count_words(results)
    return: !natural_language
        nl: Het resultaat is 10
        en: The result is 10"""
        ]
    ]
    mock_files.append(mocker.mock_open(read_data="{}").return_value)
    mock_files.append(mocker.mock_open().return_value)
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)

    translated_yaml, _ = translate_file(Path("suite.yaml"), "en")
    yaml_object = parse_yaml(translated_yaml)

    assert s.call_count == 1
    assert isinstance(yaml_object, dict)
    tabs = yaml_object["tabs"]
    assert isinstance(tabs, list)
    assert tabs[0]["testcases"][0] == {"statement": "results = Tries(10)"}
    assert tabs[0]["testcases"][1] == {
        "expression": "count_words(results)",
        "return": "The result is 10",
    }

    # Check if the file was opened for writing
    mock_opener.assert_any_call(Path("suite-en.yaml"), "w", encoding="utf-8")


def test_key_not_found(mocker: MockerFixture):
    s = mocker.spy(
        tested.nat_translation, name="generate_new_yaml"  # type: ignore[reportAttributeAccessIssue]
    )

    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in [
            """
tabs:
- tab: task3
  testcases:
  - statement: !natural_language
        nl: resultaten = Proberen({{ten}})
        en: results = Tries({{ten}})
  - expression: !natural_language
        nl: tel_woorden(resultaten)
        en: count_words(results)
    return: !natural_language
        nl: Het resultaat is 10
        en: The result is 10"""
        ]
    ]
    mock_files.append(mocker.mock_open(read_data="{}").return_value)
    mock_files.append(mocker.mock_open().return_value)
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)

    _, messages = translate_file(Path("suite.yaml"), "en")

    assert messages
    assert s.call_count == 1
    assert isinstance(messages[0], ExtendedMessage)
    assert (
        messages[0].description
        == "The natural translator found the key ten, that was not defined in the corresponding translations maps!"
    )
    assert messages[0].permission == Permission.STAFF

    # Check if the file was opened for writing
    mock_opener.assert_any_call(Path("suite-en.yaml"), "w", encoding="utf-8")


def test_parsing_failed():
    yaml_str = """
tabs:
- tab: animals
  testcases:
  - expression: tests(11)
    return: 11
  expression: calculator(1 + 1)
  - return: 2
        """.strip()
    try:
        parse_yaml(yaml_str)
    except yaml.MarkedYAMLError:
        print("As expected")
    else:
        assert False, "Expected MarkedYAMLError error"


def test_to_yaml_object():
    yaml_str = """
translations:
  animal:
    en: "animals"
    nl: "dieren"
  result:
    en: "results"
    nl: "resultaten"
  elf:
    en: "eleven"
    nl: "elf"
  select:
    en: "select"
    nl: "selecteer"
tabs:
  - tab: "{{animal}}"
    contexts:
      - testcases:
        - expression: !natural_language
            en: "count"
            nl: "tellen"
          return: !natural_language
            en: !expression 'count'
            nl: !expression 'tellen'
        - expression: 'ok(10)'
          return: !oracle
            value: !natural_language
              en: "The {{result}} 10 is OK!"
              nl: "Het {{result}} 10 is OK!"
            oracle: "custom_check"
            file: "test.py"
            name: "evaluate_test"
            arguments: !natural_language
              en: ["The value", "is OK!", "is not OK!"]
              nl: ["Het {{result}}", "is OK!", "is niet OK!"]
    """

    environment = create_enviroment()
    parsed_yaml = parse_yaml(yaml_str)
    translated_dsl = translate_yaml(parsed_yaml, {}, "en", environment)
    translated_yaml_string = convert_to_yaml(translated_dsl)
    yaml_object = _parse_yaml(translated_yaml_string)
    assert isinstance(yaml_object, dict)
    tabs = yaml_object["tabs"]
    assert isinstance(tabs, list)
    context = tabs[0]["contexts"][0]
    assert isinstance(context, dict)
    testcase = context["testcases"][0]
    assert isinstance(testcase, dict)
    assert isinstance(testcase["return"], ExpressionString)

    testcase = context["testcases"][1]
    assert isinstance(testcase, dict)
    assert isinstance(testcase["return"], ReturnOracle)


def test_dumper():
    translated_dsl = {"__tag__": "!tag", "value": [1, 2, 3]}
    yaml_str = convert_to_yaml(translated_dsl).strip()
    expected = """
!tag
- 1
- 2
- 3
    """.strip()
    assert yaml_str == expected

    translated_dsl = {"__tag__": "!tag", "value": {"key1": "value1", "key2": "value2"}}
    yaml_str = convert_to_yaml(translated_dsl).strip()
    expected = """
!tag
key1: value1
key2: value2
        """.strip()
    assert yaml_str == expected

    translated_dsl = {"__tag__": "!tag", "value": "value"}
    yaml_str = convert_to_yaml(translated_dsl).strip()
    expected = "!tag 'value'"
    assert yaml_str == expected


def test_run_is_correct_when_no_file():
    try:
        translate_file(Path("suite.yaml"), "en")
    except FileNotFoundError:
        print("As expected")
    else:
        assert False, "Expected FileNotFoundError error"


def test_template_syntax_error():
    yaml_str = """
translations:
    works:
        en: "works"
        nl: "werkt"
tabs:
- tab: animals
  testcases:
  - expression: tests(11)
    return: 11{%{{works}}
""".strip()
    translated_yml = """
tabs:
- tab: animals
  testcases:
  - expression: tests(11)
    return: 11{%{{works}}
""".strip()
    validate_natural_translate(yaml_str, translated_yml)


def test_return_json_schema():
    json_schema = {
        "return": {
            "description": "Expected return value",
            "oneOf": [
                {"$ref": "#/definitions/returnOutputChannel"},
                {
                    "type": "object",
                    "required": ["__tag__", "value"],
                    "properties": {
                        "__tag__": {
                            "type": "string",
                            "description": "The tag used in the yaml",
                            "const": "!natural_language",
                        },
                        "value": {
                            "type": "object",
                            "additionalProperties": {
                                "$ref": "#/definitions/returnOutputChannel"
                            },
                        },
                    },
                },
            ],
        }
    }

    json_schema_expected = {
        "return": {
            "description": "Expected return value",
            "anyOf": [
                {"$ref": "#/definitions/returnOutputChannel"},
                {
                    "type": "object",
                    "additionalProperties": {
                        "$ref": "#/definitions/returnOutputChannel"
                    },
                    "not": {
                        "anyOf": [
                            {"required": ["description"]},
                            {"required": ["value"]},
                            {"required": ["types"]},
                        ]
                    },
                },
            ],
        }
    }

    result = transform_ide(json_schema)

    assert result == json_schema_expected


def test_yaml_value_json_schema():
    json_schema = {
        "yamlValue": {
            "description": "A value represented as YAML.",
            "not": {"properties": {"__tag__": {"type": "string"}}, "type": "object"},
        },
    }

    json_schema_expected = {
        "yamlValue": {
            "description": "A value represented as YAML.",
        }
    }

    result = transform_ide(json_schema)

    assert result == json_schema_expected


def test_nat_lang_json_schema():
    json_schema = {
        "type": "object",
        "required": ["__tag__", "value"],
        "properties": {
            "__tag__": {
                "type": "string",
                "description": "The tag used in the yaml",
                "const": "!natural_language",
            },
            "value": {"type": "object", "additionalProperties": {"type": "string"}},
        },
    }

    json_schema_expected = {
        "type": "object",
        "additionalProperties": {"type": "string"},
        "not": {
            "anyOf": [
                {"required": ["description"]},
                {"required": ["value"]},
                {"required": ["types"]},
            ]
        },
    }

    result = transform_ide(json_schema)

    assert result == json_schema_expected


def test_expr_json_schema():
    json_schema = {
        "type": "object",
        "required": ["__tag__", "value"],
        "properties": {
            "__tag__": {
                "type": "string",
                "description": "The tag used in the yaml",
                "const": "!expression",
            },
            "value": {
                "type": "string",
                "format": "tested-dsl-expression",
                "description": "An expression in Python-syntax.",
            },
        },
    }

    json_schema_expected = {}

    result = transform_ide(json_schema)

    assert result == json_schema_expected


def test_list_json_schema():
    json_schema = [
        {},
        {
            "type": "string",
            "description": "A statement of expression in Python-like syntax as YAML string.",
        },
    ]

    json_schema_expected = [
        {
            "type": "string",
            "description": "A statement of expression in Python-like syntax as YAML string.",
        }
    ]

    result = transform_ide(json_schema)

    assert result == json_schema_expected


def test_transform_executed_correct(mocker: MockerFixture):
    s = mocker.spy(
        tested.transform_json, name="transform_ide"  # type: ignore[reportAttributeAccessIssue]
    )

    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in [
            """
{        
    "files" : {
      "description" : "A list of files used in the test suite.",
      "oneOf" : [
        {
          "type" : "array",
          "items" : {
            "$ref" : "#/definitions/file"
          }
        },
        {
          "type" : "object",
          "required": [
            "__tag__",
            "value"
          ],
          "properties" : {
            "__tag__": {
              "type" : "string",
              "description" : "The tag used in the yaml",
              "const":  "!natural_language"
            },
            "value":{
              "type": "object",
              "additionalProperties": {
                "type" : "array",
                "items" : {
                  "$ref" : "#/definitions/file"
                }
              }
            }
          }
        }
      ]
    }
}"""
        ]
    ]
    mock_files.append(mocker.mock_open(read_data="{}").return_value)
    mock_files.append(mocker.mock_open().return_value)
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)

    transform_json(Path("schema.json"), False, False)

    assert s.call_count == 25

    # Check if the file was opened for writing
    mock_opener.assert_any_call(Path("multilingual-schema.json"), "w", encoding="utf-8")


def test_json_rm_nat_lang_json_schema():
    json_schema = {
        "oneOf": [
            {"type": "array", "minItems": 1, "items": {"$ref": "#/definitions/tab"}},
            {
                "type": "object",
                "required": ["__tag__", "value"],
                "properties": {
                    "__tag__": {
                        "type": "string",
                        "description": "The tag used in the yaml",
                        "const": "!natural_language",
                    },
                    "value": {
                        "type": "object",
                        "additionalProperties": {
                            "type": "array",
                            "minItems": 1,
                            "items": {"$ref": "#/definitions/tab"},
                        },
                    },
                },
            },
        ]
    }

    json_schema_expected = {
        "type": "array",
        "minItems": 1,
        "items": {"$ref": "#/definitions/tab"},
    }

    result = transform_monolingual(json_schema, True)

    assert result == json_schema_expected


def test_json_rm_prog_lang_json_schema():
    json_schema = {
        "expressionOrStatement": {
            "oneOf": [
                {
                    "type": "string",
                    "format": "tested-dsl-expression",
                    "description": "A statement of expression in Python-like syntax as YAML string.",
                },
                {
                    "type": "object",
                    "description": "Programming-language-specific statement or expression.",
                    "minProperties": 1,
                    "propertyNames": {"$ref": "#/definitions/programmingLanguage"},
                    "items": {
                        "type": "string",
                        "description": "A language-specific literal, which will be used verbatim.",
                    },
                },
                {
                    "type": "object",
                    "required": ["__tag__", "value"],
                    "properties": {
                        "__tag__": {
                            "type": "string",
                            "description": "The tag used in the yaml",
                            "const": "!programming_language",
                        },
                        "value": {
                            "type": "object",
                            "description": "Programming-language-specific statement or expression.",
                            "minProperties": 1,
                            "propertyNames": {
                                "$ref": "#/definitions/programmingLanguage"
                            },
                            "items": {
                                "type": "string",
                                "description": "A language-specific literal, which will be used verbatim.",
                            },
                        },
                    },
                },
            ]
        },
    }

    json_schema_expected = {
        "expressionOrStatement": {
            "oneOf": [
                {
                    "type": "string",
                    "format": "tested-dsl-expression",
                    "description": "A statement of expression in Python-like syntax as YAML string.",
                },
                {
                    "description": "Programming-language-specific statement or expression.",
                    "minProperties": 1,
                    "propertyNames": {"$ref": "#/definitions/programmingLanguage"},
                    "items": {
                        "type": "string",
                        "description": "A language-specific literal, which will be used verbatim.",
                    },
                    "anyOf": [{"type": "object"}, {"type": "programming_language"}],
                },
            ]
        },
    }

    result = transform_monolingual(json_schema, True)

    assert result == json_schema_expected


def test_strict_json_schema_oracle():
    json_schema = {
        "oneOf": [
            {"$ref": "#/definitions/yamlValueOrPythonExpression"},
            {
                "type": "object",
                "required": ["__tag__", "value"],
                "properties": {
                    "__tag__": {
                        "type": "string",
                        "description": "The tag used in the yaml",
                        "const": "!oracle",
                    },
                    "value": {
                        "type": "object",
                        "additionalProperties": False,
                        "required": ["value"],
                        "properties": {
                            "oracle": {"const": "builtin"},
                            "value": {
                                "oneOf": [
                                    {
                                        "$ref": "#/definitions/yamlValueOrPythonExpression"
                                    },
                                    {
                                        "type": "object",
                                        "required": ["__tag__", "value"],
                                        "properties": {
                                            "__tag__": {
                                                "type": "string",
                                                "description": "The tag used in the yaml",
                                                "const": "!natural_language",
                                            },
                                            "value": {
                                                "type": "object",
                                                "additionalProperties": {
                                                    "$ref": "#/definitions/yamlValueOrPythonExpression"
                                                },
                                            },
                                        },
                                    },
                                ]
                            },
                        },
                    },
                },
            },
        ]
    }

    json_schema_expected = {
        "oneOf": [
            {"$ref": "#/definitions/yamlValueOrPythonExpression"},
            {
                "type": "oracle",
                "additionalProperties": False,
                "required": ["value"],
                "properties": {
                    "oracle": {"const": "builtin"},
                    "value": {"$ref": "#/definitions/yamlValueOrPythonExpression"},
                },
            },
        ]
    }

    result = transform_monolingual(json_schema, True)

    assert result == json_schema_expected


def test_strict_json_schema_expression():
    json_schema = {
        "oneOf": [
            {"$ref": "#/definitions/yamlValue"},
            {
                "type": "object",
                "required": ["__tag__", "value"],
                "properties": {
                    "__tag__": {
                        "type": "string",
                        "description": "The tag used in the yaml",
                        "const": "!expression",
                    },
                    "value": {
                        "type": "string",
                        "format": "tested-dsl-expression",
                        "description": "An expression in Python-syntax.",
                    },
                },
            },
        ]
    }

    json_schema_expected = {
        "oneOf": [
            {"$ref": "#/definitions/yamlValue"},
            {
                "type": "expression",
                "format": "tested-dsl-expression",
                "description": "An expression in Python-syntax.",
            },
        ]
    }

    result = transform_monolingual(json_schema, True)
    print(result)

    assert result == json_schema_expected


def test_json_schema_edge_cases():
    json_schema = {
        "expressionOrStatementWithNatTranslation": {},
        "translations": {},
        "$ref": "#/definitions/expressionOrStatementWithNatTranslation",
        "yamlValue": {
            "description": "A value represented as YAML.",
            "not": {"properties": {"__tag__": {"type": "string"}}, "type": "object"},
        },
    }

    json_schema_expected = {
        "$ref": "#/definitions/expressionOrStatement",
        "yamlValue": {
            "description": "A value represented as YAML.",
            "not": {"type": ["oracle", "expression", "programming_language"]},
        },
    }

    result = transform_monolingual(json_schema, True)

    assert result == json_schema_expected

    json_schema = {
        "yamlValue": {
            "description": "A value represented as YAML.",
            "not": {"properties": {"__tag__": {"type": "string"}}, "type": "object"},
        },
    }

    json_schema_expected = {
        "yamlValue": {
            "description": "A value represented as YAML.",
        }
    }

    result = transform_monolingual(json_schema, False)

    assert result == json_schema_expected
