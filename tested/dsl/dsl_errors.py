import sys
import textwrap

import yaml
from jsonschema.exceptions import ValidationError

from tested.dodona import ExtendedMessage, Permission


class InvalidYamlError(ValueError):
    pass


class DslValidationError(ValueError):
    pass


def convert_validation_error_to_group(
    error: ValidationError,
) -> ExceptionGroup | Exception:
    if not error.context and not error.cause:
        if len(error.message) > 150:
            message = error.message.replace(str(error.instance), "<DSL>")
            note = "With <DSL> being: " + textwrap.shorten(str(error.instance), 500)
        else:
            message = error.message
            note = None
        converted = DslValidationError(
            f"Validation error at {error.json_path}: " + message
        )
        if note:
            converted.add_note(note)
        return converted
    elif error.cause:
        return error.cause
    elif error.context:
        causes = [convert_validation_error_to_group(x) for x in error.context]
        message = f"Validation error at {error.json_path}, caused by a sub-exception."
        return ExceptionGroup(message, causes)
    else:
        return error


def handle_dsl_validation_errors(errors: list):
    if len(errors) == 1:
        message = (
            "Validating the DSL resulted in an error. "
            "The most specific sub-exception is often the culprit. "
        )
        error = convert_validation_error_to_group(errors[0])
        if isinstance(error, ExceptionGroup):
            raise ExceptionGroup(message, error.exceptions)
        else:
            raise DslValidationError(message + str(error)) from error
    elif len(errors) > 1:
        the_errors = [convert_validation_error_to_group(e) for e in errors]
        message = "Validating the DSL resulted in some errors."
        raise ExceptionGroup(message, the_errors)


def raise_yaml_error(yaml_stream: str, exc: yaml.MarkedYAMLError):
    lines = yaml_stream.splitlines()

    if exc.problem_mark is None:
        # There is no additional information, so what can we do?
        raise exc

    sys.stderr.write(
        textwrap.dedent(
            f"""
            YAML error while parsing test suite. This means there is a YAML syntax error.

            The YAML parser indicates the problem lies at line {exc.problem_mark.line + 1}, column {exc.problem_mark.column + 1}:

                {lines[exc.problem_mark.line]}
                {" " * exc.problem_mark.column + "^"}

            The error message was:
                {exc.problem} {exc.context}

            The detailed exception is provided below.
            You might also find help by validating your YAML file with a YAML validator.\n
            """
        )
    )
    raise exc


def build_preprocessor_messages(
    translations_missing_key: list[str],
) -> list[ExtendedMessage]:
    """
    Build the preprocessor messages from the missing keys.

    :param translations_missing_key: The missing keys.
    :return: The preprocessor messages.
    """
    return [
        ExtendedMessage(
            f"The natural translator found the key {key}, that was not defined in the corresponding translations maps!",
            permission=Permission.STAFF,
        )
        for key in translations_missing_key
    ]
