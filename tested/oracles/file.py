from attr import evolve

from tested.dodona import Status, StatusMessage
from tested.internationalization import get_i18n_string
from tested.oracles.common import OracleConfig, OracleResult
from tested.oracles.text import compare_text
from tested.testsuite import ContentPath, FileOutputChannel, OutputChannel, TextData


def _file_defaults(config: OracleConfig) -> dict:
    defaults = {
        "mode": "full",
        "ignoreWhitespace": False,
        "caseInsensitive": False,
        "tryFloatingPoint": False,
        "applyRounding": False,
        "roundTo": 3,
    }
    defaults.update(config.options)
    if defaults["mode"] not in ("line", "full"):
        raise ValueError(f"Unknown mode for file oracle: {defaults['mode']}")
    return defaults


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
    options = _file_defaults(config)

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
) -> OracleResult:
    assert isinstance(
        file.path, str
    ), "File path must be a string when using file evaluator"

    try:
        expected_content = file.get_data_as_string(config.bundle.config.resources)
    except FileNotFoundError:
        # We know content is ContentPath if we get a file not found error.
        assert isinstance(file.content, ContentPath)
        return OracleResult(
            result=StatusMessage(
                enum=Status.INTERNAL_ERROR,
                human=get_i18n_string("oracles.text.file.not-found"),
            ),
            readable_expected=file.content.path,
            readable_actual="",
            channel_override=file.path,
        )

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
        text_result = compare_text(options, expected_content, actual)
        return evolve(text_result, channel_override=file.path)
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
