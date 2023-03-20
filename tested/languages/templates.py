import dataclasses
import logging
from pathlib import Path
from typing import Any, Dict, List

from mako import exceptions
from mako.exceptions import TemplateLookupException
from mako.lookup import TemplateLookup
from mako.template import Template

from tested.configs import Bundle
from tested.languages import get_language
from tested.languages._preprocessors import remove_indents

_logger = logging.getLogger(__name__)


def _write_template(arguments, template: Template, path: Path):
    """
    Write a template with the arguments as a data class.

    :param arguments: The arguments for the template. Should be a dataclass.
    :param template: The template to write.
    :param path: Where to write the template to.
    """
    # We cannot use dataclasses.asdict, since that will recurse, which
    # won't work properly for functions and attributes.
    fields = dataclasses.fields(arguments)
    values = {field.name: getattr(arguments, field.name) for field in fields}
    try:
        result = template.render(**values)
    except Exception as e:
        _logger.error(exceptions.text_error_template().render())
        raise e
    # noinspection PyTypeChecker
    with open(path, "w") as file:
        file.write(result)


def _language_inheritance_tree(bundle: Bundle) -> List[str]:
    current = bundle.lang_config
    result = [bundle.config.programming_language]
    while lang := current.inherits_from():
        result.append(lang)
        current = get_language(lang)
    return result


def path_to_templates(bundle: Bundle) -> List[Path]:
    """
    Construct the paths to the folder containing the templates files for a given
    programming language (passed via the configs pack).

    :param bundle: The configuration bundle.

    :return: A list of template folders.
    """
    judge_root = bundle.config.judge
    result = []
    for language in _language_inheritance_tree(bundle):
        result.append(judge_root / "tested" / "languages" / language / "templates")
    assert result, "At least one template folder is required."
    return result


_env_cache: Dict[str, TemplateLookup] = {}


def _get_environment(bundle: Bundle) -> TemplateLookup:
    """Get the templating environment for a given configuration bundle."""
    if bundle.config.programming_language not in _env_cache:
        paths = [str(x) for x in path_to_templates(bundle)]
        template = TemplateLookup(directories=paths, preprocessor=[remove_indents])
        _env_cache[bundle.config.programming_language] = template

    return _env_cache[bundle.config.programming_language]


def find_and_write_template(
    bundle: Bundle, template_args: Any, destination: Path, template_name: str
) -> str:
    """
    Find and write using a template.

    :param bundle: The configuration bundle.
    :param template_args: The template arguments.
    :param destination: Where the resulting file should be written. If the
                        destination is a folder, the name of the template will be
                        used as a file name. If the destination is a file name, it
                        will be used instead.
    :param template_name: The name of the template to use.

    :return: The name of the generated file.
    """
    template_name = bundle.lang_config.conventionalize_namespace(template_name)
    if destination.is_dir():
        destination /= bundle.lang_config.with_extension(template_name)

    template = find_template(bundle, template_name)
    _write_template(template_args, template, destination)
    return destination.name


def find_template(bundle: Bundle, template_name: str) -> Template:
    """
    Find a template with a given name. The function will attempt to find a template
    with the given name and any of the allowed extensions for the given language.
    Will raise if nothing is found.

    :param bundle: The configuration bundle.
    :param template_name: The name of the template.

    :return: A Mako template.

    :raises LookupError: If no template for the given name were found.
    """
    error = None
    environment = _get_environment(bundle)
    for extension in bundle.lang_config.extension_templates():
        try:
            file_name = f"{template_name}.{extension}"
            return environment.get_template(file_name)
        except TemplateLookupException as e:
            error = e
    raise LookupError(
        f"Could not find template with name {template_name} for language "
        f"{bundle.config.programming_language}",
        error,
        environment.directories,
    )
