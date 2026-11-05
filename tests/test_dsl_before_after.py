from pathlib import Path

import pytest

from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.parametrize(
    ("language", "suite"), [("python", "python.yaml"), ("c", "c.yaml")]
)
def test_before_seeds_the_random_generator(
    language: str, suite: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "seeded-random", language, tmp_path, suite, "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert len(updates.find_all("start-context")) == 2
    assert updates.find_status_enum() == ["correct"] * 5
