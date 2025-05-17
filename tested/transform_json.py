import json
import os
import sys
from pathlib import Path
from typing import Any

SPECIAL_TYPES = ["expression", "programming_language", "oracle"]


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


def make_tag_structure(
    data: Any, data_with_inner_translations: Any = None, tag: str = "!natural_language"
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

    if tag in ["!oracle", "!programming_language", "!expression"]:
        base["properties"]["value"] = data
        base["properties"]["value"]["type"] = "object"
        if tag == "!expression":
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
    return {"type": "object", "description": "Define translations in the global scope."}


def transform_json_for_preprocessor_validation(data: Any, in_sub_def: bool) -> Any:
    """
    This function is responsible for transforming the JSON-schema such that translations are supported.
    It also uses a special structure for tags in the YAML that is used in the preprocessing step.

    :param data: The data to transform.
    :param in_sub_def: Indicates if the sub-definition are being processed.
    :return: The transformed data.
    """
    if isinstance(data, dict):
        # Standard creation of the special tag structure for translations.
        new_data = {
            key: transform_json_for_preprocessor_validation(
                value, in_sub_def or key == "subDefinitions"
            )
            for key, value in data.items()
        }
        if (
            "type" in data
            and (data["type"] != "object" or in_sub_def)
            and data["type"] not in ["boolean", "integer"]
        ):
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

        # Add the translations maps
        targets = ["_rootObject", "tab", "unit", "context", "case"]
        for target in targets:
            if target in data and "properties" in data[target]:
                data[target]["properties"]["translations"] = make_translations_map()

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
        return [
            transform_json_for_preprocessor_validation(value, in_sub_def)
            for value in data
        ]
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
        result = transform_json_for_preprocessor_validation(json_stream, False)
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
