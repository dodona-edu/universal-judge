from functools import partial
from typing import cast

from attr import dataclass
from jinja2 import Template
from marko import Markdown

from tested.configs import Bundle
from tested.datatypes import AdvancedTypes, AllTypes, string_to_type
from tested.descriptions.renderer import TestedRenderer, render_one_statement
from tested.features import TypeSupport
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
from tested.utils import get_args


@dataclass
class Datatype:
    locale: str
    language: Language
    type_: AllTypes
    others: tuple[NestedTypeDeclaration]

    def _types(self) -> list[str]:
        if len(self.others):
            result = [
                generate_type_declaration(self.language, (self.type_, self.others))
            ]
        elif isinstance(self.type_, AdvancedTypes):
            result = [generate_type_declaration(self.language, self.type_)]
        else:
            possible_types = [self.type_]
            supported_types = self.language.datatype_support()
            for advanced_type_enum in get_args(AdvancedTypes):
                for advanced_type in advanced_type_enum:
                    if (
                        advanced_type.base_type == self.type_
                        and supported_types.get(advanced_type) == TypeSupport.SUPPORTED
                    ):
                        possible_types.append(advanced_type)

            all_types = {
                generate_type_declaration(self.language, x) for x in possible_types
            }
            result = sorted(all_types)

        assert (
            len(result) > 0
        ), f"Could not find concrete type for {self.type_} in {self.language.__class__.__name__}"
        return result

    def __str__(self) -> str:
        types = self._types()
        types = [f"`{x}`" for x in types]
        last_sep = f" {get_i18n_string('types.joiner')} "
        return last_sep.join(
            [", ".join(types[:-1]), types[-1]] if len(types) > 2 else types
        )

    @property
    def simple(self) -> str:
        if len(self.others):
            return generate_type_declaration(self.language, (self.type_, self.others))
        else:
            return generate_type_declaration(self.language, self.type_)

    @property
    def singular(self) -> str:
        return get_i18n_string(f"types.singular.{self.type_}")

    @property
    def plural(self) -> str:
        return get_i18n_string(f"types.singular.{self.type_}")


def construct_datatype(
    locale: str, language: Language, type_: str, *others: NestedTypeDeclaration
) -> Datatype:
    enum_type = string_to_type(type_)
    # noinspection PyTypeChecker
    return Datatype(locale=locale, language=language, type_=enum_type, others=others)


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
    language = bundle.language
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
        datatype=partial(construct_datatype, bundle.config.natural_language, language),
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
