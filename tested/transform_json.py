import json
import os
import sys
from pathlib import Path
from typing import Any


def transform(data: Any) -> Any:
    if isinstance(data, list):
        return [transformed for ele in data if (transformed := transform(ele)) != {}]
    elif isinstance(data, dict):
        if "return" in data:
            # This is necessary since tags aren't recognized in the Json schema
            # and such natural_language maps wil always be seen as yamlValue.
            assert isinstance(data["return"], dict) and "oneOf" in data["return"]
            data["return"]["anyOf"] = data["return"].pop("oneOf")

        if "yamlValue" in data:
            data["yamlValue"] = {
                "description": "A value represented as YAML.",
            }

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

                if (
                    tag["const"] == "!programming_language"
                    or tag["const"] == "!expression"
                ):
                    return {}

                if tag["const"] == "!natural_language":
                    value["not"] = {
                        "anyOf": [
                            {"required": ["description"]},
                            {"required": ["value"]},
                            {"required": ["types"]},
                        ]
                    }
                data = value

        return {k: transform(v) for k, v in data.items()}
    return data


def transform_json(json_file):
    _, ext = os.path.splitext(json_file)
    assert ext.lower() == ".json", f"expected a json file, got {ext}."
    try:
        with open(json_file, "r") as stream:
            json_stream = json.load(stream)
    except FileNotFoundError as e:
        print("The json file was not found.")
        raise e

    result = transform(json_stream)

    print(json.dumps(result, indent=2))
    with open("output.json", "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
    # print(result)


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected path to multilingual json schema."

    transform_json(Path(sys.argv[1]))
