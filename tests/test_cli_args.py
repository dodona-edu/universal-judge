from pathlib import Path

import pytest

from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize("language", ["haskell"])
def test_cli_params(language: str, tmp_path: Path, pytestconfig: pytest.Config):
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
