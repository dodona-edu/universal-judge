from pathlib import Path

from pytest_mock import MockerFixture

import tested
from tested.transform_json import (
    transform_ide,
    transform_json,
    transform_json_for_preprocessor_validation,
)


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
            "oneOf": [
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

    json_schema_expected = {
        "type": "string",
        "format": "tested-dsl-expression",
        "description": "An expression in Python-syntax.",
    }

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
        tested.transform_json, name="transform_json_preprocessor"  # type: ignore[reportAttributeAccessIssue]
    )

    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in [
            """
{        
    "files": {
      "oneOf": [
        {
          "description": "A list of files used in the test suite.",
          "type": "array",
          "items": {
            "$ref": "#/subDefinitions/file"
          }
        },
        {
          "type": "object",
          "required": [
            "__tag__",
            "value"
          ],
          "properties": {
            "__tag__": {
              "type": "string",
              "description": "The tag used in the yaml",
              "const": "!natural_language"
            },
            "value": {
              "type": "object",
              "additionalProperties": {
                "description": "A list of files used in the test suite.",
                "type": "array",
                "items": {
                  "$ref": "#/subDefinitions/file"
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

    transform_json(Path("schema.json"), True, False)

    assert s.call_count == 25

    # Check if the file was opened for writing
    mock_opener.assert_any_call(
        Path("schema-strict-nat-translation.json"), "w", encoding="utf-8"
    )


def test_nat_lang_json_schema_structure():
    json_schema = {
        "type": "array",
        "minItems": 1,
        "items": {"$ref": "#/definitions/tab"},
    }

    json_schema_expected = {
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

    result = transform_json_for_preprocessor_validation(json_schema, False)

    assert result == json_schema_expected


def test_prog_lang_json_schema_structure():
    json_schema = {
        "expressionOrStatement": {
            "type": "programming_language",
            "description": "Programming-language-specific statement or expression.",
            "minProperties": 1,
            "propertyNames": {"$ref": "#/subDefinitions/programmingLanguage"},
            "items": {
                "type": "string",
                "description": "A language-specific literal, which will be used verbatim.",
            },
        },
    }

    json_schema_expected = {
        "expressionOrStatement": {
            "oneOf": [
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
                            "description": "Programming-language-specific statement or expression.",
                            "minProperties": 1,
                            "propertyNames": {
                                "$ref": "#/subDefinitions/programmingLanguage"
                            },
                            "items": {
                                "oneOf": [
                                    {
                                        "type": "string",
                                        "description": "A language-specific literal, which will be used verbatim.",
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
                                                    "type": "string",
                                                    "description": "A language-specific literal, which will be used verbatim.",
                                                },
                                            },
                                        },
                                    },
                                ]
                            },
                            "type": "object",
                        },
                    },
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
                                "type": "object",
                                "required": ["__tag__", "value"],
                                "properties": {
                                    "__tag__": {
                                        "type": "string",
                                        "description": "The tag used in the yaml",
                                        "const": "!programming_language",
                                    },
                                    "value": {
                                        "description": "Programming-language-specific statement or expression.",
                                        "minProperties": 1,
                                        "propertyNames": {
                                            "$ref": "#/subDefinitions/programmingLanguage"
                                        },
                                        "items": {
                                            "type": "string",
                                            "description": "A language-specific literal, which will be used verbatim.",
                                        },
                                        "type": "object",
                                    },
                                },
                            },
                        },
                    },
                },
            ]
        }
    }

    result = transform_json_for_preprocessor_validation(json_schema, True)

    assert result == json_schema_expected


def test_json_schema_oracle():
    json_schema = {
        "type": "oracle",
        "additionalProperties": False,
        "required": ["value"],
        "properties": {
            "oracle": {"const": "builtin"},
            "value": {"$ref": "#/subDefinitions/yamlValueOrPythonExpression"},
        },
    }

    json_schema_expected = {
        "oneOf": [
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
                        "additionalProperties": False,
                        "required": ["value"],
                        "properties": {
                            "oracle": {"const": "builtin"},
                            "value": {
                                "oneOf": [
                                    {
                                        "$ref": "#/subDefinitions/yamlValueOrPythonExpression"
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
                                                    "$ref": "#/subDefinitions/yamlValueOrPythonExpression"
                                                },
                                            },
                                        },
                                    },
                                ]
                            },
                        },
                        "type": "object",
                    },
                },
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
                            "type": "object",
                            "required": ["__tag__", "value"],
                            "properties": {
                                "__tag__": {
                                    "type": "string",
                                    "description": "The tag used in the yaml",
                                    "const": "!oracle",
                                },
                                "value": {
                                    "additionalProperties": False,
                                    "required": ["value"],
                                    "properties": {
                                        "oracle": {"const": "builtin"},
                                        "value": {
                                            "$ref": "#/subDefinitions/yamlValueOrPythonExpression"
                                        },
                                    },
                                    "type": "object",
                                },
                            },
                        },
                    },
                },
            },
        ]
    }

    result = transform_json_for_preprocessor_validation(json_schema, True)

    assert result == json_schema_expected


def test_json_schema_expression():
    json_schema = {
        "yamlValueOrPythonExpression": {
            "oneOf": [
                {"$ref": "#/definitions/yamlValue"},
                {
                    "type": "expression",
                    "format": "tested-dsl-expression",
                    "description": "An expression in Python-syntax.",
                },
            ]
        }
    }

    json_schema_expected = {
        "yamlValueOrPythonExpression": {
            "oneOf": [
                {"$ref": "#/subDefinitions/yamlValue"},
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
    }

    result = transform_json_for_preprocessor_validation(json_schema, True)
    assert result == json_schema_expected


def test_json_schema_yaml_value():
    json_schema = {
        "yamlValue": {
            "description": "A value represented as YAML.",
        }
    }

    json_schema_expected = {
        "yamlValue": {
            "description": "A value represented as YAML.",
            "not": {"properties": {"__tag__": {"type": "string"}}, "type": "object"},
        },
    }

    result = transform_json_for_preprocessor_validation(json_schema, False)

    assert result == json_schema_expected


def test_exception_when_file_not_found():

    try:
        transform_json(Path("test.json"), False, False)
    except FileNotFoundError:
        print("As expected")
    else:
        assert False, "Expected FileNotFoundError error"
