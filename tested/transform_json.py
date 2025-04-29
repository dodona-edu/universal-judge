import json
import os
import sys
from enum import StrEnum
from pathlib import Path
from typing import Any


class SpecialMap(StrEnum):
    NATURAL_LANGUAGE = "natural_language"
    PROGRAMMING_LANGUAGE = "programming_language"
    ORACLE = "oracle"
    EXPRESSION = "expression"
    NONE = "none"


def map_kind(element: dict) -> SpecialMap:
    if "required" in element and "properties" in element:
        required = element["required"]
        properties = element["properties"]
        assert isinstance(required, list)
        assert isinstance(properties, dict)
        if "__tag__" in required and "value" in required:
            if "__tag__" in properties and "value" in properties:
                tag = properties["__tag__"]
                if (
                    isinstance(tag, dict)
                    and "const" in tag
                    and isinstance(tag["const"], str)
                ):
                    if tag["const"] == "!natural_language":
                        return SpecialMap.NATURAL_LANGUAGE
                    elif tag["const"] == "!programming_language":
                        return SpecialMap.PROGRAMMING_LANGUAGE
                    elif tag["const"] == "!oracle":
                        return SpecialMap.ORACLE
                    elif tag["const"] == "!expression":
                        return SpecialMap.EXPRESSION
    return SpecialMap.NONE


def change_prog_lang_type(element: dict, prog_lang: dict) -> dict:
    if element == prog_lang:
        ele_type = element.pop("type")
        element["anyOf"] = [
            {"type": ele_type},
            {"type": "programming_language"},
        ]
    return element


def transform_monolingual(data: Any, strict_schema: bool) -> Any:
    if isinstance(data, dict):
        if "oneOf" in data:
            new_one_of = []
            prog_lang = None
            for element in data["oneOf"]:
                assert isinstance(element, dict)
                # Removes all natural translations
                kind = map_kind(element)
                if kind == SpecialMap.PROGRAMMING_LANGUAGE:
                    prog_lang = element["properties"]["value"]
                elif kind != SpecialMap.NATURAL_LANGUAGE:
                    if kind == SpecialMap.EXPRESSION or kind == SpecialMap.ORACLE:
                        element = element["properties"]["value"]
                        if strict_schema:
                            if kind == SpecialMap.EXPRESSION:
                                element["type"] = "expression"
                            else:
                                element["type"] = "oracle"
                    new_one_of.append(element)

            # A programming_language map was found. If not strict, just remove.
            # If strict, still provide the type option for the corresponding object.
            if prog_lang is not None and strict_schema:
                new_one_of = [
                    change_prog_lang_type(ele, prog_lang) for ele in new_one_of
                ]

            if len(new_one_of) <= 1:
                data.pop("oneOf")
                if len(new_one_of) == 1:
                    for key, value in new_one_of[0].items():
                        data[key] = value
            else:
                data["oneOf"] = new_one_of

        # The next changes are a few edge cases.
        if "expressionOrStatementWithNatTranslation" in data:
            data.pop("expressionOrStatementWithNatTranslation")

        if "translations" in data:
            data.pop("translations")

        if "$ref" in data:
            if isinstance(data["$ref"], str):
                if (
                    data["$ref"]
                    == "#/definitions/expressionOrStatementWithNatTranslation"
                ):
                    data["$ref"] = "#/definitions/expressionOrStatement"

        if "yamlValue" in data:
            if strict_schema:
                data["yamlValue"] = {
                    "description": "A value represented as YAML.",
                    "not": {"type": ["oracle", "expression", "programming_language"]},
                }
            else:
                data["yamlValue"] = {
                    "description": "A value represented as YAML.",
                }

        return {k: transform_monolingual(v, strict_schema) for k, v in data.items()}
    elif isinstance(data, list):
        return [
            transformed
            for ele in data
            if (transformed := transform_monolingual(ele, strict_schema)) != {}
        ]
    return data


def transform_ide(data: Any) -> Any:
    if isinstance(data, list):
        return [
            transformed for ele in data if (transformed := transform_ide(ele)) != {}
        ]
    elif isinstance(data, dict):
        if "return" in data:
            # This is necessary since tags aren't recognized in the Json schema.
            # So a natural_language maps wil always be seen as yamlValue.
            assert isinstance(data["return"], dict) and "oneOf" in data["return"]
            data["return"]["anyOf"] = data["return"].pop("oneOf")

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
                if (
                    tag["const"] == "!programming_language"
                    or tag["const"] == "!expression"
                ):
                    return {}

                # This to help the validator to somewhat distinguish between
                # a natural_language map and a dictionary.
                if tag["const"] == "!natural_language":
                    value["not"] = {
                        "anyOf": [
                            {"required": ["description"]},
                            {"required": ["value"]},
                            {"required": ["types"]},
                        ]
                    }
                data = value

        return {k: transform_ide(v) for k, v in data.items()}
    return data


def transform_json(json_file: Path, monolingual: bool, strict: bool):
    """
    This function transforms the JSON schema used in the DSL translator into
    a new JSON schema that can be used to validate the multilingual YAML in your IDE.

    :param json_file: The path to the JSON file.
    :param monolingual: indicator if it will generate a monolingual schema.
    :param strict: indicator if it will generate a strict schema. This only applies
    to monolingual transforms
    """
    _, ext = os.path.splitext(json_file)
    assert ext.lower() == ".json", f"expected a json file, got {ext}."
    try:
        with open(json_file, "r") as stream:
            json_stream = json.load(stream)
    except FileNotFoundError as e:
        print("The json file was not found.")
        raise e

    if not monolingual:
        result = transform_ide(json_stream)
        file_name = "multilingual-schema.json"
    else:
        result = transform_monolingual(json_stream, strict)
        if strict:
            file_name = "schema-strict.json"
        else:
            file_name = "schema.json"

    with open(json_file.parent / file_name, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected path to multilingual json schema."
    monolingual = False
    strict = False
    if n > 2:
        assert sys.argv[2] in ("strict", "not-strict")
        strict = sys.argv[2] == "strict"

        monolingual = True

    transform_json(Path(sys.argv[1]), monolingual=monolingual, strict=strict)
