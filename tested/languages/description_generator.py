from __future__ import annotations

import html
import json
import logging
import os
from pathlib import Path
from typing import Union, Tuple, List, Optional, Dict

from pygments import highlight
from pygments.formatters.html import HtmlFormatter
from pygments.lexers import get_lexer_by_name

from ..configs import Bundle
from ..dsl import parse_string

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .config import Language

TYPE_ARG = Union[str, Tuple[str, Union[List["TYPE_ARG"], "TYPE_ARG"]]]

TYPE_CONFIG_NAME = Optional[Dict[str, Dict[str, Union[str, Dict[str, str]]]]]

logger = logging.getLogger(__name__)

_html_formatter = HtmlFormatter(nowrap=True)

_console_lexer = get_lexer_by_name("console")


def highlight_console(stmt):
    """
    Highlight a console statement.
    """
    return highlight(stmt, _console_lexer, _html_formatter)


class DescriptionGenerator:
    __slots__ = ["language", "types", "_lexer"]

    def __init__(
        self, language: Language, config_dir: Path, types_file: str = "types.json"
    ):
        self.language = language
        path_to_types = config_dir / types_file
        if not os.path.exists(path_to_types):
            path_to_types = config_dir.parent / language.inherits_from() / types_file

        with open(path_to_types, "r") as f:
            self.types = json.load(f)

        self._lexer = get_lexer_by_name(self.types["console"]["name"], stripall=True)

    def get_natural_type_name(
        self, type_name: str, bundle: Bundle, plural: bool = False, is_html: bool = True
    ) -> str:
        try:
            group = self.types["natural"]["plural" if plural else "singular"]
            value = group[bundle.config.natural_language][type_name]
        except KeyError:
            value = type_name
        return html.escape(value) if is_html else value

    def get_type_name(
        self,
        args: TYPE_ARG,
        bundle: Bundle,
        custom_type_map: TYPE_CONFIG_NAME = None,
        is_inner: bool = False,
        is_html: bool = True,
        recursive_call: bool = False,
    ) -> str:
        programming_language = bundle.config.programming_language

        def _get_type(arg: str) -> Union[str, bool]:
            try:
                return custom_type_map[programming_language][args]
            except KeyError:
                return self.types[arg]

        def _get_type_or_conventionalize(arg: str) -> str:
            try:
                return _get_type(arg)
            except KeyError:
                return self.language.conventionalize_class(arg)

        def _get_type_name(arg: str) -> Union[str, bool]:
            if not is_inner:
                return _get_type_or_conventionalize(arg)
            else:
                try:
                    return custom_type_map[programming_language]["inner"][arg]
                except KeyError:
                    try:
                        return self.types["inner"][arg]
                    except KeyError:
                        return _get_type_or_conventionalize(arg)

        if custom_type_map is None:
            custom_type_map = dict()

        if isinstance(args, str):
            name = _get_type_name(args)
            type_name = name if isinstance(name, str) else args
        else:
            main_type = _get_type_name(args[0])
            if isinstance(args[1], str):
                types = [
                    self.get_type_name(
                        args[1], bundle, custom_type_map, bool(main_type), is_html, True
                    )
                ]
            elif isinstance(args[1], list):
                types = [
                    self.get_type_name(
                        arg, bundle, custom_type_map, bool(main_type), is_html, True
                    )
                    for arg in args[1]
                ]
            else:
                types = [
                    self.get_type_name(
                        args[1], bundle, custom_type_map, bool(main_type), is_html, True
                    )
                ]
            if isinstance(main_type, str):
                type_name = (
                    f"{self.types[args[0]]}"
                    f"{self.types['brackets']['open']}"
                    f"{', '.join(types)}{self.types['brackets']['close']}"
                )
            elif main_type:
                type_name = (
                    f"{self.types['brackets'][args[0]]['open']}"
                    f"{', '.join(types)}"
                    f"{self.types['brackets'][args[0]]['close']}"
                )
            elif len(types) == 1:
                type_name = (
                    f"{types[0]}{self.types['brackets'][args[0]]['open']}"
                    f"{self.types['brackets'][args[0]]['close']}"
                )
            else:
                raise ValueError(f"Type {main_type} expects only one subtype")
        if is_html and not recursive_call:
            return html.escape(type_name)
        return type_name

    def get_function_name(self, name: str, is_html: bool = True) -> str:
        function_name = self.language.conventionalize_function(name)
        if is_html:
            return html.escape(function_name)
        return function_name

    def get_property_name(self, name: str, is_html: bool = True) -> str:
        name = self.language.conventionalize_property(name)
        if is_html:
            return html.escape(name)
        return name

    def get_variable_name(self, name: str, is_html: bool = True) -> str:
        name = self.language.conventionalize_identifier(name)
        if is_html:
            return html.escape(name)
        return name

    def get_global_variable_name(self, name: str, is_html: bool = True) -> str:
        name = self.language.conventionalize_global_identifier(name)
        if is_html:
            return html.escape(name)
        return name

    def get_code(
        self, stmt: str, bundle: Bundle, statement: bool = False, is_html: bool = True
    ) -> str:
        from .generator import convert_statement

        if statement:
            stmt = parse_string(stmt)
        else:
            stmt = parse_string(stmt, is_return=True)

        required = stmt.get_used_features()
        available = self.language.supported_constructs()

        if not (required.constructs <= available):
            logger.warning("This statement is not compatible!")
            logger.warning(f"Required constructs are {required.constructs}.")
            logger.warning(f"The language supports {available}.")
            missing = (required.constructs ^ available) & required.constructs
            logger.warning(f"Missing features are: {missing}.")
            raise Exception("Missing features")

        stmt = convert_statement(bundle, stmt)
        stmt = self.language.cleanup_description(bundle.suite.namespace, stmt)
        if is_html:
            prompt = html.escape(self.types["console"]["prompt"]).strip()
            stmt = self.generate_html_code(stmt).strip()
            return (prompt + " " if statement else "") + stmt
        else:
            return (
                (self.types["console"]["prompt"].strip() + " " if statement else "")
                + stmt
            ).strip()

    def generate_html_code(self, stmt: str) -> str:
        return highlight(stmt, self._lexer, _html_formatter)

    def get_prompt(self, is_html: bool = True):
        value = self.types["console"]["prompt"]
        return html.escape(value) if is_html else value

    def get_prompt_language(self, is_html: bool = True):
        value = self.types["console"]["name"]
        return html.escape(value) if is_html else value
