from abc import ABC, abstractmethod
from functools import partial
from typing import Callable, cast

from attr import dataclass
from jinja2 import Environment, FileSystemLoader
from marko import Markdown
from typing_extensions import override

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
from tested.serialisation import Identifier
from tested.testsuite import LanguageMapping
from tested.utils import get_args


class Datatype(ABC):
    @abstractmethod
    def __str__(self) -> str:
        raise NotImplementedError()

    @property
    @abstractmethod
    def simple(self) -> str:
        raise NotImplementedError()

    @property
    @abstractmethod
    def singular(self) -> str:
        raise NotImplementedError()

    @property
    @abstractmethod
    def plural(self) -> str:
        raise NotImplementedError()


@dataclass
class GenericDatatype(Datatype):
    locale: str
    language: Language
    type_: AllTypes
    others: tuple[NestedTypeDeclaration, ...]

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

    @override
    def __str__(self) -> str:
        types = self._types()
        types = [f"`{x}`" for x in types]
        last_sep = f" {get_i18n_string('types.joiner')} "
        return last_sep.join(
            [", ".join(types[:-1]), types[-1]] if len(types) > 2 else types
        )

    @property
    @override
    def simple(self) -> str:
        if len(self.others):
            return generate_type_declaration(self.language, (self.type_, self.others))
        else:
            return generate_type_declaration(self.language, self.type_)

    @property
    @override
    def singular(self) -> str:
        return get_i18n_string(f"types.singular.{self.type_}")

    @property
    @override
    def plural(self) -> str:
        return get_i18n_string(f"types.singular.{self.type_}")


@dataclass
class LanguageSpecificDatatype(Datatype):
    result: str

    @override
    def __str__(self) -> str:
        return self.result

    @property
    @override
    def simple(self) -> str:
        return self.result

    @property
    @override
    def singular(self) -> str:
        return self.result

    @property
    @override
    def plural(self) -> str:
        return self.result


def _construct_datatype(
    bundle: Bundle, type_: str | LanguageMapping, *others: NestedTypeDeclaration
) -> Datatype:
    if isinstance(type_, dict):
        assert bundle.config.programming_language in type_, (
            f"Cannot convert to {bundle.config.programming_language}, as a language-specific"
            f" type was request while not provided. The provided languages are {type_.keys()}."
        )
        return LanguageSpecificDatatype(
            result=type_[bundle.config.programming_language]
        )
    else:
        enum_type = string_to_type(type_)
        # noinspection PyTypeChecker
        return GenericDatatype(
            locale=bundle.config.natural_language,
            language=bundle.language,
            type_=enum_type,
            others=others,
        )


def _support_language_specific_arguments(
    normal: Callable[[Language, Identifier], Identifier],
    bundle: Bundle,
    actual: str | LanguageMapping,
) -> str:
    if isinstance(actual, dict):
        assert bundle.config.programming_language in actual, (
            f"Cannot convert to {bundle.config.programming_language}, as a language-specific"
            f" construct was request while not provided. The provided languages are {actual.keys()}."
        )
        return actual[bundle.config.programming_language]
    else:
        return normal(bundle.language, Identifier(actual))


def convert_templated_problem(bundle: Bundle, raw_description: str) -> str:
    """
    Render a Mako problem into a normal problem.

    :param bundle: The bundle of the programming language to convert to.
    :param raw_description: The raw, Mako description.
    :return: The processed (Markdown) description.
    """
    environment = Environment(
        loader=FileSystemLoader(
            searchpath=bundle.config.resources,
        ),
        autoescape=False,
        keep_trailing_newline=True,
    )
    description_template = environment.from_string(source=raw_description)
    set_locale(bundle.config.natural_language)
    return description_template.render(
        # Conventionalize functions
        namespace=partial(
            _support_language_specific_arguments, conventionalize_namespace, bundle
        ),
        function=partial(
            _support_language_specific_arguments, conventionalize_function, bundle
        ),
        identifier=partial(
            _support_language_specific_arguments, conventionalize_identifier, bundle
        ),
        property=partial(
            _support_language_specific_arguments, conventionalize_property, bundle
        ),
        clazz=partial(
            _support_language_specific_arguments, conventionalize_class, bundle
        ),
        global_identifier=partial(
            _support_language_specific_arguments,
            conventionalize_global_identifier,
            bundle,
        ),
        # Access to the current programming language
        programming_language=bundle.config.programming_language,
        # Data type conversion
        datatype=partial(_construct_datatype, bundle),
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
