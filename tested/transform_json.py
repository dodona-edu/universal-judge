import copy
import json
import os
import sys
from pathlib import Path
from typing import Any

SPECIAL_TYPES = ["expression", "programming_language", "oracle", "parameter"]


def transform_non_strict(data: Any) -> Any:
    """
    This function is responsible for transforming the strict JSON-schema into its non-strict form.

    :param data: The data to transform.
    :return: The transformed data.
    """
    if isinstance(data, list):
        return [
            transformed
            for ele in data
            if (transformed := transform_non_strict(ele)) != {}
        ]
    elif isinstance(data, dict):
        if "type" in data and data["type"] in SPECIAL_TYPES:
            if data["type"] == "expression":
                data["type"] = "string"
            elif data["type"] == "oracle":
                data["type"] = "object"
            elif data["programming_language"]:
                return {}

        return {k: transform_non_strict(v) for k, v in data.items()}
    return data


def transform_ide(data: Any) -> Any:
    """
    This function will work from the result of transform_json_for_preprocessor_validation.
    It will transform the JSON-schema into a non-strict form that does not use the special form
    for tags. This way the JSON-schema can be used in the IDE to help the developer write correct DSL.

    :param data: The data to transform.
    :return: The transformed data.
    """
    if isinstance(data, list):
        return [
            transformed for ele in data if (transformed := transform_ide(ele)) != {}
        ]
    elif isinstance(data, dict):
        if "yamlValue" in data:
            data["yamlValue"] = {
                "description": "A value represented as YAML.",
            }

        # This is a speciale structure for tags.
        if "required" in data and "properties" in data:
            required = data["required"]
            properties = data["properties"]
            assert isinstance(required, list)
            assert isinstance(properties, dict)
            if "__tag__" in required and "value" in required:
                assert "__tag__" in properties and "value" in properties
                value = properties["value"]
                tag = properties["__tag__"]
                assert isinstance(value, dict)
                assert isinstance(tag, dict) and "const" in tag

                # For !programming_language the same was already defined without the tag.
                # The only usage for the expression tag is also redundant.
                if tag["const"] == "!programming_language":
                    return {}

                value = {k: transform_ide(v) for k, v in value.items()}
                if (
                    "additionalProperties" in value
                    and value["additionalProperties"] == {}
                ):
                    return {}

                # This to help the validator to somewhat distinguish between
                # a natural_language map and another YAML-map.
                if tag["const"] == "!natural_language":
                    value["not"] = {
                        "anyOf": [
                            {"required": ["description"]},
                            {"required": ["value"]},
                            {"required": ["types"]},
                        ]
                    }

                if "oneOf" in value and value["oneOf"] == []:
                    return {}

                return value

        data = {k: transform_ide(v) for k, v in data.items()}
        if "oneOf" in data and data["oneOf"] == []:
            return {}
        return data
    return data


def flatten_one_of_stack(data: list) -> list:
    new_one_stack = []
    for ele in data:
        if isinstance(ele, dict) and len(ele.keys()) == 1 and "oneOf" in ele:
            assert isinstance(ele["oneOf"], list)
            new_one_stack.extend(ele["oneOf"])
        else:
            new_one_stack.append(ele)
    return new_one_stack


def make_parameter_type_structure(data: dict) -> dict:
    return {
        "anyOf": [
            {"type": "parameter", "description": "The key of the parameter."},
            data,
        ]
    }


def make_tag_structure(
    data: dict, data_with_inner_translations: Any = None, tag: str = "!natural_language"
) -> dict:
    base = {
        "type": "object",
        "required": ["__tag__", "value"],
        "properties": {
            "__tag__": {
                "type": "string",
                "description": "The tag used in the yaml",
                "const": tag,
            },
        },
    }

    if tag in ["!oracle", "!programming_language", "!expression", "!parameter"]:
        base["properties"]["value"] = data
        base["properties"]["value"]["type"] = "object"
        if tag in ["!expression", "!parameter"]:
            base["properties"]["value"]["type"] = "string"

    elif tag == "!natural_language":
        base["properties"]["value"] = {
            "type": "object",
            "additionalProperties": data,
        }
        if data_with_inner_translations is not None:
            return {"oneOf": [data_with_inner_translations, base]}

    return base


def make_translations_map() -> dict:
    return {"type": "object", "description": "Define translations."}


def make_templates_map() -> dict:
    return {
        "type": "object",
        "description": "Define templates.",
        "additionalProperties": {
            "$ref": "#/definitions/testcase_without_templates",
        },
    }


def transform_json_for_preprocessor_validation(data: Any) -> Any:
    """
    This function will start with the result of add_parameter_type.
    It is responsible for transforming the JSON-schema such that translations and templates are supported.
    It also uses a special structure for tags in the YAML that is used in the preprocessing step.

    :param data: The data to transform.
    :return: The transformed data.
    """
    if isinstance(data, dict):
        # Add the translations and templates maps
        targets = ["_rootObject", "tab", "unit", "context", "case"]
        for target in targets:
            if target in data and "properties" in data[target]:
                data[target]["properties"]["translations"] = make_translations_map()
                data[target]["properties"]["templates"] = make_templates_map()

        new_data = {
            key: (
                transform_json_for_preprocessor_validation(value)
                if key != "translations" and key != "templates"
                else value
            )
            for key, value in data.items()
        }

        # Standard creation of the special tag structure for translations and templates.
        if "type" in data and data["type"] not in ["boolean", "integer"]:
            # Adding support for the other tags.
            if data["type"] in SPECIAL_TYPES:
                tag = data.pop("type")
                new_data.pop("type")
                # translations applied to inner part
                tag_data_with_inner_translations = make_tag_structure(
                    new_data,
                    tag=f"!{tag}",
                )
                # translations not applied to inner part
                tag_data = make_tag_structure(
                    data,
                    tag=f"!{tag}",
                )
                return make_tag_structure(
                    data=tag_data,
                    data_with_inner_translations=tag_data_with_inner_translations,
                )

            return make_tag_structure(data, new_data)
        data = new_data

        # Flatten the oneOf structures
        if "oneOf" in data:
            assert isinstance(data["oneOf"], list)
            data["oneOf"] = flatten_one_of_stack(data["oneOf"])

        # Edge cases for return
        if "yamlValueOrPythonExpression" in data:
            data["yamlValueOrPythonExpression"] = {
                "oneOf": [
                    {"$ref": "#/subDefinitions/yamlValue"},
                    {
                        "type": "object",
                        "required": ["__tag__", "value"],
                        "properties": {
                            "__tag__": {
                                "type": "string",
                                "description": "The tag used in the yaml",
                                "const": "!parameter",
                            },
                            "value": {
                                "type": "string",
                                "description": "The key of the parameter.",
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

        if "yamlValue" in data:
            data["yamlValue"] = {
                "description": "A value represented as YAML.",
                "not": {
                    "properties": {"__tag__": {"type": "string"}},
                    "type": "object",
                },
            }

        if (
            "$ref" in data
            and data["$ref"] == "#/subDefinitions/yamlValueOrPythonExpression"
        ):
            data = make_tag_structure(data, data)
        return data

    if isinstance(data, list):
        return [transform_json_for_preprocessor_validation(value) for value in data]
    return data


def add_templates(data: dict) -> dict:
    """
    This function will start by a new definition for a single testcase and add templates to the existing definition.
    The new definition for a testcase will be used to specify a testcase inside a template.

    :param data: The data to transform.
    :return: The transformed data.
    """
    assert "definitions" in data and isinstance(data["definitions"], dict)
    # Addition for adding templates to the json-schema
    testcase = data["definitions"].get("testcase", {})
    if testcase:
        assert "properties" in testcase
        testcase_without_templates = copy.deepcopy(testcase)
        testcase_without_templates["properties"]["arguments"] = (
            make_parameter_type_structure(
                testcase_without_templates["properties"]["arguments"]
            )
        )
        data["definitions"]["testcase_without_templates"] = testcase_without_templates

        testcase["properties"]["template"] = {
            "type": "string",
            "description": "Name of the template to insert.",
        }
        testcase["properties"]["parameters"] = {
            "type": "object",
            "description": "The parameters that are to be inserted into the template.",
            "additionalProperties": {
                "$ref": "#/subDefinitions/yamlValueOrPythonExpression"
            },
        }
        testcase["properties"]["repeat"] = {
            "type": "object",
            "description": "A certain loop that will generate test cases with the given parameters and template.",
            "required": ["template", "parameters"],
            "properties": {
                "template": {
                    "type": "string",
                    "description": "Name of the template to insert.",
                },
                "parameters": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "description": "The parameters that are to be inserted into the template.",
                        "additionalProperties": {
                            "$ref": "#/subDefinitions/yamlValueOrPythonExpression"
                        },
                    },
                },
            },
        }

        data["definitions"]["testcase"] = testcase
    return data


def add_parameter_type(data: Any) -> Any:
    """
    This function will start with the result of add_templates and add the potential to use the type
    parameter in specific places.

    :param data: The data to transform.
    :return: The transformed data.
    """
    if isinstance(data, dict):
        if "type" in data:
            type_value = data["type"]
            if (
                isinstance(type_value, list)
                and any(t in ["boolean", "integer", "number"] for t in type_value)
            ) or type_value in ["boolean", "integer", "number"]:
                # Adding support for "!parameter" tag.
                return make_parameter_type_structure(data)
        return {k: add_parameter_type(v) for k, v in data.items()}

    if isinstance(data, list):
        return [add_parameter_type(value) for value in data]
    return data


def transform_json(json_file: Path, multilingual: bool, ide: bool):
    """
    This function transforms the JSON schema used in the DSL translator into
    a new JSON schema that can be used to validate the multilingual YAML in your IDE.

    :param json_file: The path to the JSON file.
    :param multilingual: indicator if it will generate a multilingual schema.
    :param ide: indicates if it will generate a JSON-schema for the IDE of the user.
    """
    _, ext = os.path.splitext(json_file)
    assert ext.lower() == ".json", f"expected a json file, got {ext}."
    try:
        with open(json_file, "r") as stream:
            json_stream = json.load(stream)
    except FileNotFoundError as e:
        print("The json file was not found.")
        raise e

    if not multilingual:
        result = transform_non_strict(json_stream)
        file_name = "schema.json"
    else:
        stream_with_templates = add_templates(json_stream)
        stream_with_templates_and_param = add_parameter_type(stream_with_templates)
        result = transform_json_for_preprocessor_validation(
            stream_with_templates_and_param
        )
        if ide:
            result = transform_ide(result)
            file_name = "multilingual-schema.json"
        else:
            file_name = "schema-strict-nat-translation.json"

    with open(json_file.parent / file_name, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)


if __name__ == "__main__":
    n = len(sys.argv)
    assert n >= 1

    multilingual_param = False
    ide_param = False
    if n >= 2:
        assert sys.argv[1] in ("validation", "ide")
        multilingual_param = True
        ide_param = sys.argv[1] == "ide"

    transform_json(
        Path("./tested/dsl/schema-strict.json"),
        multilingual=multilingual_param,
        ide=ide_param,
    )
