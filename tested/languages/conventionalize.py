"""
Functions to conventionalize various aspects of a programming langauge.
"""
import logging
from typing import TYPE_CHECKING, Callable, Dict, Literal

if TYPE_CHECKING:
    from tested.languages import Language

_logger = logging.getLogger(__name__)


EXECUTION_PREFIX = "execution"


def camel_snake_case(what: str) -> str:
    """
    Convert a string to camel_Snake_Case from snake_case. The algorithm is
    simple: each letter behind an underscore will be capitalized. The first
    letter will be downcased.

    >>> camel_snake_case("__foo_bar__")
    '__Foo_Bar__'
    >>> camel_snake_case("this_is_snake_case")
    'this_Is_Snake_Case'
    >>> camel_snake_case("_Weird_cases_aRe_mostly_KEPT")
    '_Weird_Cases_ARe_Mostly_KEPT'
    >>> camel_snake_case("numbers_1_2_are_not_special")
    'numbers_1_2_Are_Not_Special'
    >>> camel_snake_case("________________")
    '________________'

    :param what: The string to convert.
    :return: The converted string.
    """
    result = pascal_snake_case(what)
    return (result[0].lower() + result[1:]) if result else ""


def camelize(what: str) -> str:
    """
    Convert a string to camelCase from snake_case. The algorithm is simple: each
    underscore is removed, and the letter behind it will be capitalized. The first
    letter will be downcased.

    >>> camelize("__foo_bar__")
    'fooBar'
    >>> camelize("this_is_snake_case")
    'thisIsSnakeCase'
    >>> camelize("_Weird_cases_aRe_mostly_KEPT")
    'weirdCasesAReMostlyKEPT'
    >>> camelize("numbers_1_2_are_not_special")
    'numbers12AreNotSpecial'
    >>> camelize("________________")
    ''

    :param what: The string to convert.
    :return: The converted string.
    """
    result = pascalize(what)
    return (result[0].lower() + result[1:]) if result else ""


def cobol_case(what: str) -> str:
    """
    Convert a string to Train-Case from snake_case. The algorithm is simple:
    each underscore is replaced by a dash, and all letters will becapitalized.
    The first letter will be downcased.

    >>> cobol_case("__foo_bar__")
    '--FOO-BAR--'
    >>> cobol_case("this_is_snake_case")
    'THIS-IS-SNAKE-CASE'
    >>> cobol_case("_Weird_cases_aRe_mostly_KEPT")
    '-WEIRD-CASES-ARE-MOSTLY-KEPT'
    >>> cobol_case("numbers_1_2_are_not_special")
    'NUMBERS-1-2-ARE-NOT-SPECIAL'
    >>> cobol_case("________________")
    '----------------'

    :param what: The string to convert.
    :return: The converted string.
    """
    return pascal_snake_case(what).replace("_", "-").upper()


def dash_case(what: str) -> str:
    """
    Convert a string to doner|case from snake_case. The algorithm is simple:
    each underscore is replaced by a dash.

    >>> dash_case("__foo_bar__")
    '--foo-bar--'
    >>> dash_case("this_is_snake_case")
    'this-is-snake-case'
    >>> dash_case("_Weird_cases_aRe_mostly_KEPT")
    '-Weird-cases-aRe-mostly-KEPT'
    >>> dash_case("numbers_1_2_are_not_special")
    'numbers-1-2-are-not-special'
    >>> dash_case("________________")
    '----------------'

    :param what: The string to convert.
    :return: The converted string.
    """
    return what.replace("_", "-")


def doner_case(what: str) -> str:
    """
    Convert a string to doner|case from snake_case. The algorithm is simple:
    each underscore is replaced by a `|`.

    >>> doner_case("__foo_bar__")
    '||foo|bar||'
    >>> doner_case("this_is_snake_case")
    'this|is|snake|case'
    >>> doner_case("_Weird_cases_aRe_mostly_KEPT")
    '|Weird|cases|aRe|mostly|KEPT'
    >>> doner_case("numbers_1_2_are_not_special")
    'numbers|1|2|are|not|special'
    >>> doner_case("________________")
    '||||||||||||||||'

    :param what: The string to convert.
    :return: The converted string.
    """
    return what.replace("_", "|")


def flat_case(what: str) -> str:
    """
    Convert a string to flatcase from snake_case. The algorithm is simple:
    each underscore is removed.

    >>> flat_case("__foo_bar__")
    'foobar'
    >>> flat_case("this_is_snake_case")
    'thisissnakecase'
    >>> flat_case("_Weird_cases_aRe_mostly_KEPT")
    'WeirdcasesaRemostlyKEPT'
    >>> flat_case("numbers_1_2_are_not_special")
    'numbers12arenotspecial'
    >>> flat_case("________________")
    ''

    :param what: The string to convert.
    :return: The converted string.
    """
    return what.replace("_", "")


def macro_case(what: str) -> str:
    """
    Convert a string to MACRO_CASE from snake_case. The algorithm is simple:
    the string will be capitalized.

    >>> macro_case("__foo_bar__")
    '__FOO_BAR__'
    >>> macro_case("this_is_snake_case")
    'THIS_IS_SNAKE_CASE'
    >>> macro_case("_Weird_cases_aRe_mostly_KEPT")
    '_WEIRD_CASES_ARE_MOSTLY_KEPT'
    >>> macro_case("numbers_1_2_are_not_special")
    'NUMBERS_1_2_ARE_NOT_SPECIAL'
    >>> macro_case("________________")
    '________________'

    :param what: The string to convert.
    :return: The converted string.
    """
    return what.upper()


def pascal_snake_case(what: str) -> str:
    """
    Convert a string to Pascal_Snake_Case from snake_case. The algorithm is
    simple: each letter behind the underscore will be capitalized. The first
    letter will be downcased.

    >>> pascal_snake_case("__foo_bar__")
    '__Foo_Bar__'
    >>> pascal_snake_case("this_is_snake_case")
    'This_Is_Snake_Case'
    >>> pascal_snake_case("_Weird_cases_aRe_mostly_KEPT")
    '_Weird_Cases_ARe_Mostly_KEPT'
    >>> pascal_snake_case("numbers_1_2_are_not_special")
    'Numbers_1_2_Are_Not_Special'
    >>> pascal_snake_case("________________")
    '________________'

    :param what: The string to convert.
    :return: The converted string.
    """
    result = ""
    i = 0
    while i < len(what):
        this = what[i]
        if this == "_":
            i += 1
            result += "_"
            if i < len(what):
                while i < len(what) - 1 and what[i] == "_":
                    i += 1
                    result += "_"
                r = what[i]
                if r != "_":
                    result += r.upper()
                else:
                    result += "_"
        else:
            result += this
        i += 1

    return (result[0].upper() + result[1:]) if result else ""


def pascalize(what: str) -> str:
    """
    Convert a string to PascalCase from snake_case.

    >>> pascalize("__foo_bar__")
    'FooBar'
    >>> pascalize("this_is_snake_case")
    'ThisIsSnakeCase'
    >>> pascalize("_Weird_cases_aRe_mostly_KEPT")
    'WeirdCasesAReMostlyKEPT'
    >>> pascalize("numbers_1_2_are_not_special")
    'Numbers12AreNotSpecial'
    >>> pascalize("________________")
    ''

    :param what: The string to convert.
    :return: The converted string.
    """
    return pascal_snake_case(what).replace("_", "")


def snake_case(what: str) -> str:
    """
    Emits a warning if the string is not in snake_case. The check is simple: it
    just checks for capitals.

    :param what: The name.
    :return: The same name.
    """
    if any(x.isupper() for x in what):
        _logger.warning(
            f"A name '{what}' is not in snake_case. This might cause problems."
        )
    return what


def train_case(what: str) -> str:
    """
    Convert a string to Train-Case from snake_case. The algorithm is simple: each
    underscore is replaced by a dash, and the letter behind it will be capitalized.
    The first letter will be downcased.

    >>> train_case("__foo_bar__")
    '--Foo-Bar--'
    >>> train_case("this_is_snake_case")
    'This-Is-Snake-Case'
    >>> train_case("_Weird_cases_aRe_mostly_KEPT")
    '-Weird-Cases-ARe-Mostly-KEPT'
    >>> train_case("numbers_1_2_are_not_special")
    'Numbers-1-2-Are-Not-Special'
    >>> train_case("________________")
    '----------------'

    :param what: The string to convert.
    :return: The converted string.
    """
    return pascal_snake_case(what).replace("_", "-")


def upper_flat_case(what: str) -> str:
    """
    Convert a string to UPPERFLATCASE from snake_case. The algorithm is simple:
    each underscore is removed, and all letters will becapitalized.

    >>> upper_flat_case("__foo_bar__")
    'FOOBAR'
    >>> upper_flat_case("this_is_snake_case")
    'THISISSNAKECASE'
    >>> upper_flat_case("_Weird_cases_aRe_mostly_KEPT")
    'WEIRDCASESAREMOSTLYKEPT'
    >>> upper_flat_case("numbers_1_2_are_not_special")
    'NUMBERS12ARENOTSPECIAL'
    >>> upper_flat_case("________________")
    ''

    :param what: The string to convert.
    :return: The converted string.
    """
    return flat_case(what).upper()


Conventionable = Literal[
    "namespace", "function", "identifier", "property", "class", "global_identifier"
]

NamingConventions = Literal[
    "camel_case",
    "camel_snake_case",
    "cobol_case",
    "dash_case",
    "donor_case",
    "flat_case",
    "macro_case",
    "pascal_case",
    "pascal_snake_case",
    "snake_case",
    "train_case",
    "upper_flat_case",
]


_case_mapping: Dict[NamingConventions, Callable[[str], str]] = {
    "camel_case": camelize,
    "camel_snake_case": camel_snake_case,
    "cobol_case": cobol_case,
    "dash_case": dash_case,
    "donor_case": doner_case,
    "flat_case": flat_case,
    "macro_case": macro_case,
    "pascal_case": pascalize,
    "pascal_snake_case": pascal_snake_case,
    "snake_case": snake_case,
    "train_case": train_case,
    "upper_flat_case": upper_flat_case,
}


def _conventionalize(
    language: "Language", what: Conventionable, identifier: str
) -> str:
    conventions = language.naming_conventions()
    mapper = _case_mapping[conventions.get(what, "snake_case")]
    return mapper(identifier)


def conventionalize_class(language: "Language", class_name: str) -> str:
    """
    Conventionalize a class name.

    This will convert the given class name (which should be snake case) to the
    convention used by the programming language. By default, snake case will
    be used.
    """
    return _conventionalize(language, "class", class_name)


def conventionalize_function(language: "Language", function_name: str) -> str:
    """
    Conventionalize the name of a function.

    This will convert the given function name (which should be snake case)
    to the convention used by the programming language. By default, snake case
    will be used.
    """
    return _conventionalize(language, "function", function_name)


def conventionalize_identifier(language: "Language", identifier: str) -> str:
    """
    Conventionalize the name of an identifier.

    This will convert the given identifier (which should be snake case) to the
    convention used by the programming language. By default, snake case will
    be used.
    """
    return _conventionalize(language, "identifier", identifier)


def conventionalize_global_identifier(language: "Language", identifier: str) -> str:
    """
    Conventionalize the name of a global identifier.

    This will convert the given global identifier (which should be snake case)
    to the convention used by the programming language. By default, snake case
    will be used.
    """
    return _conventionalize(language, "global_identifier", identifier)


def conventionalize_namespace(language: "Language", namespace: str) -> str:
    """
    Conventionalize a namespace.

    This will convert the given namespace (which should be snake case)
    to the convention used by the programming language. By default, snake case
    will be used.

    A namespace's meaning depends on the programming language, but is generally
    the namespace in which a function resides. For example, in Java it might
    be a class name for static functions, while in Haskell it means the module
    name.
    """
    return _conventionalize(language, "namespace", namespace)


def conventionalize_property(language: "Language", property_name: str) -> str:
    """
    Conventionalize the name of a property.

    This will convert the given property_name (which should be snake case)
    to the convention used by the programming language. By default, snake case
    will be used.
    """

    return _conventionalize(language, "property", property_name)


def submission_name(language: "Language") -> str:
    """
    :return: The name of a submission.
    """
    return conventionalize_namespace(language, language.config.suite.namespace)


def submission_file(language: "Language") -> str:
    """
    :return: The file name of a submission.
    """
    return language.with_extension(submission_name(language))


def selector_name(language: "Language") -> str:
    """
    :return: The name for the selector, conventionalized.
    """
    return conventionalize_namespace(language, "selector")


def selector_file(language: "Language") -> str:
    """
    :return: The name for the selector, conventionalized.
    """
    return language.with_extension(selector_name(language))


def execution_name(language: "Language", tab_number: int, execution_number: int) -> str:
    """
    Get the name of an execution. The name should be unique for the tab and
    execution number combination.

    :param language: The language module.
    :param tab_number: The number of the tab.
    :param execution_number: The number of the execution.
    :return: The name of the execution.
    """
    name = f"{EXECUTION_PREFIX}_{tab_number}_{execution_number}"
    return conventionalize_namespace(language, name)
