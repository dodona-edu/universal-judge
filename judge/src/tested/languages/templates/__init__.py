import dataclasses
from functools import lru_cache
from pathlib import Path
from typing import List, Any

from mako import exceptions
from mako.exceptions import TemplateLookupException
from mako.lookup import TemplateLookup
from mako.template import Template

from languages.generator import _logger
from languages.templates._preprocessors import remove_indents, remove_newline
from tested import Bundle


def _write_template(arguments, template: Template, path: Path):
    """
    Write a template with the arguments as a data class.

    :param arguments: The arguments for the template. Should be a dataclass.
    :param template: The template to write.
    :param path: Where to write the template to.
    """
    try:
        result = template.render(**dataclasses.asdict(arguments))
    except Exception as e:
        _logger.error(exceptions.text_error_template().render())
        raise e
    with open(path, "w") as file:
        file.write(result)


def path_to_templates(bundle: Bundle) -> List[Path]:
    """
    Construct the paths to the folder containing the templates files for a given
    programming language (passed via the config pack).

    :param bundle: The configuration bundle.

    :return: A list of template folders.
    """
    judge_root = bundle.config.judge
    language = bundle.config.programming_language
    result = []
    for end in bundle.language_config.template_folders(language):
        result.append(judge_root / 'judge' / 'config' / 'templates' / end)
    assert result, "At least one template folder is required."
    return result


@lru_cache
def get_environment(bundle: Bundle) -> TemplateLookup:
    """Get the templating environment for a given configuration bundle."""
    processors = [remove_indents, remove_newline]
    paths = [str(x) for x in path_to_templates(bundle)]
    return TemplateLookup(directories=paths, preprocessor=processors)


def find_and_write_template(bundle: Bundle,
                            template_args: Any,
                            destination: Path,
                            template_name: str) -> str:
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
    template_name = bundle.language_config.conventionalise_object(template_name)
    if destination.is_dir():
        destination /= bundle.language_config.with_extension(template_name)

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
    for extension in bundle.language_config.template_extensions():
        try:
            file_name = f"{template_name}.{extension}"
            return get_environment(bundle).get_template(file_name)
        except TemplateLookupException as e:
            error = e
    raise LookupError(f"Could not find template with name {template_name}", error)
