import json
from pathlib import Path

import pytest
import yaml

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedSequenceTypes,
    BasicNumericTypes,
)
from tested.serialisation import NothingType, NumberType, SequenceType
from tested.utils import sorted_no_duplicates, sorting_value_extract
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("language", ["haskell"])
def test_cli_params(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "dedup",
        language,
        tmp_path,
        "plan.yaml",
        "solution",
    )
    result = execute_config(conf)
    print(result)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct", "correct", "correct"]
