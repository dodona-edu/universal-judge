from functools import partial
from typing import cast

from jinja2 import Template
from marko import Markdown

from tested.configs import Bundle
from tested.datatypes import AllTypes
from tested.descriptions.renderer import TestedRenderer, render_one_statement
from tested.internationalization import get_i18n_string, set_locale
from tested.languages import Language
from tested.languages.conventionalize import (
    conventionalize_class,
    conventionalize_function,
    conventionalize_global_identifier,
    conventionalize_identifier,
    conventionalize_namespace,
    conventionalize_property,
)
from tested.languages.generation import NestedTypeDeclaration, generate_type_declaration


def type_declaration(
    language: Language, type_: AllTypes, *others: NestedTypeDeclaration
) -> str:
    if len(others):
        result = generate_type_declaration(language, (type_, others))
    else:
        result = generate_type_declaration(language, type_)
    assert isinstance(result, str)
    return result


def common_type_name(type_: AllTypes, plural: bool = False):
    key = "plural" if plural else "singular"
    return get_i18n_string(f"types.{key}.{type_}")


def convert_templated_problem(bundle: Bundle, raw_description: str) -> str:
    """
    Render a Mako problem into a normal problem.

    :param bundle: The bundle of the programming language to convert to.
    :param raw_description: The raw, Mako description.
    :return: The processed (Markdown) description.
    """

    description_template = Template(
        source=raw_description, autoescape=False, keep_trailing_newline=True
    )
    language = bundle.lang_config
    set_locale(bundle.config.natural_language)
    return description_template.render(
        # Conventionalize functions
        namespace=partial(conventionalize_namespace, language),
        function=partial(conventionalize_function, language),
        identifier=partial(conventionalize_identifier, language),
        property=partial(conventionalize_property, language),
        clazz=partial(conventionalize_class, language),
        global_identifier=partial(conventionalize_global_identifier, language),
        # Access to the current programming language
        programming_language=bundle.config.programming_language,
        # Data type conversion
        datatype=partial(type_declaration, language),
        datatype_common=common_type_name,
        t=partial(render_one_statement, bundle),
    )


def convert_tested_markdown(bundle: Bundle, markdown_description: str) -> str:
    """
    Convert code blocks in the Markdown text.

    :param bundle: The configuration bundle.
    :param markdown_description: The markdown scription.
    :return: A converted description.
    """
    marko = Markdown(renderer=TestedRenderer)
    # Ugly, but needed.
    marko.parse("")
    cast(TestedRenderer, marko.renderer).bundle = bundle

    return marko.convert(markdown_description)
