import json
import sys
from typing import Union

import yaml


def parse_stream(test_context: dict, data: Union[str, dict], stream: str,
                 is_input=False):
    in_out = "input" if is_input else "output"
    if isinstance(data, str):
        test_context[in_out][stream] = {
            "type": "text",
            "data": data
        }
    elif is_input:
        raise TypeError("Input can only be a string")
    else:
        raise TypeError("Unimplemented type")


def parse_context_test(test: dict) -> dict:
    json_ctx_test = {
        "context_testcase": {
            "input":  {
                "main_call": True
            },
            "output": {
            }
        }
    }
    for ch, is_input in (("stdin", True), ("stdout", False), ("stderr", False)):
        try:
            parse_stream(json_ctx_test["context_testcase"], test[ch], ch,
                         is_input=is_input)
        except KeyError:
            pass

    return json_ctx_test


def parse_tab(tab: dict) -> dict:
    json_tab = {
        "name":     tab["tab"] if tab["tab"] else "Feedback",
        "contexts": []
    }
    try:
        json_tab["contexts"].extend(
            parse_context_test(test) for test in tab["tests"])
    except KeyError:
        pass  # Ignore exception

    return json_tab


if __name__ == "__main__":
    yaml_plan = yaml.safe_load(sys.stdin)
    json_plan = {"tabs": [parse_tab(tab) for tab in yaml_plan]}
    json.dump(json_plan, sys.stdout, indent=2)
    print()
