from tested.dodona import Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.oracles.common import OracleConfig, OracleResult
from tested.oracles.text import _text_options, compare_text
from tested.testsuite import FileOutputChannel, OutputChannel, TextData


def evaluate_file(
    config: OracleConfig, channel: OutputChannel, actual: str
) -> list[OracleResult] | OracleResult:
    """
    Evaluate the contents of two files. The file oracle supports one option,
    ``mode``, used to define in which mode the oracle should operate:

    1. ``full``: The complete contents are passed to the :class:`TextEvaluator`.
    2. ``line``: The file is split by lines and each line is compared to the
       corresponding line with the :class:`TextEvaluator`. The lines are compared
       without newlines.

    Since the text oracle is used behind the scenes, this oracle also supports
    all parameters of that oracle.

    When no mode is passed, the oracle will default to ``full``.
    """
    assert isinstance(channel, FileOutputChannel)
    options = _text_options(config)

    # There must be nothing as output.
    if actual:
        message = get_i18n_string("oracles.text.file.unexpected.message", actual=actual)
        return OracleResult(
            result=StatusMessage(
                enum=Status.WRONG,
                human=get_i18n_string("oracles.text.file.unexpected.status"),
            ),
            readable_expected="",
            readable_actual=actual,
            messages=[message],
        )

    results = []

    for file in channel.files:
        results.append(compare_file(config, file, options))

    return results


def compare_file(
    config: OracleConfig,
    file: TextData,
    options: dict,
):
    assert isinstance(
        file.path, str
    ), "File path must be a string when using file evaluator"

    try:
        expected_content = file.get_data_as_string(config.bundle.config.resources)
    except FileNotFoundError:
        raise ValueError(f"File {file.path} not found in resources.")

    actual_path = config.context_dir / file.path

    try:
        with open(str(actual_path), "r") as f:
            actual = f.read()
    except FileNotFoundError:
        return OracleResult(
            result=StatusMessage(
                enum=Status.RUNTIME_ERROR,
                human=get_i18n_string("oracles.text.file.not-found"),
            ),
            readable_expected=expected_content,
            readable_actual="",
            channel_override=file.path,
        )

    if options["mode"] == "full":
        return compare_text(options, expected_content, actual)
    else:
        assert options["mode"] == "line"
        strip_newlines = options.get("stripNewlines", False)
        expected_lines = expected_content.splitlines(keepends=not strip_newlines)
        actual_lines = actual.splitlines(keepends=not strip_newlines)
        correct = len(actual_lines) == len(expected_lines)
        for expected_line, actual_line in zip(expected_lines, actual_lines):
            r = compare_text(options, expected_line, actual_line)
            correct = correct and r.result.enum == Status.CORRECT

        return OracleResult(
            result=StatusMessage(enum=Status.CORRECT if correct else Status.WRONG),
            readable_expected=expected_content,
            readable_actual=actual,
            channel_override=file.path,
        )
