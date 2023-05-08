import json
import shutil
import sys
from argparse import ArgumentParser
from copy import deepcopy
from dataclasses import dataclass
from itertools import groupby
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

from mako.template import Template
from pydantic.json import pydantic_encoder

from tested.datatypes import BasicObjectTypes, BasicSequenceTypes
from tested.description_instance import (
    create_description_instance_from_template,
    prepare_template,
)
from tested.dsl import parse_dsl
from tested.languages import LANGUAGES, Language, get_language, language_exists
from tested.testsuite import Suite, parse_test_suite


class InstantiateError(Exception):
    pass


@dataclass
class DescriptionFile:
    location: Path
    type: str = "md"
    natural_language: str = "en"
    is_natural_language_explicit: bool = False
    is_template: bool = False
    template: Optional[Template] = None


def _analyse_description_dir(
    description_dir: Path, default_i18n: str
) -> Tuple[List[DescriptionFile], List[Path]]:
    """
    Read the description directory

    :param description_dir: Description directory to analyse
    :return: tuple of a list of description files and a list of the other files
    """
    descriptions, other = [], []
    for path in description_dir.iterdir():
        if (
            path.is_file()
            and path.name.startswith("description")
            and path.suffix.lower() in (".md", ".html", ".mako")
        ):
            file = DescriptionFile(location=path, natural_language=default_i18n)
            file.is_template = path.suffix.lower() == ".mako"
            # Natural language is given
            if len(path.suffixes) > (1 + int(file.is_template)):
                file.is_natural_language_explicit = True
                i18n_suffix = path.suffixes[-(2 + int(file.is_template))]
                # Slice to remove dot
                file.natural_language = i18n_suffix[1:].lower()
            # Slice to remove dot
            file.type = path.suffixes[-(1 + int(file.is_template))][1:].lower()

            descriptions.append(file)
        else:
            other.append(path)
    return descriptions, other


def _check_if_all_languages_exists(languages: List[str]):
    """
    Check if all languages to check exists in TESTed

    :param languages: list of the languages to test
    :return:
    """
    for language in languages:
        if not language_exists(language):
            raise InstantiateError(
                f"Programming language '{language}' isn't supported by TESTed"
            )


def _check_if_directory_exists(name: str, path: Path):
    """
    Check if the given directory exist

    :param name: Name of the directory
    :param path: Location of the directory
    :return:
    """
    if not path.exists():
        raise InstantiateError(f"{name} directory '{path}' doesn't exists")
    elif not path.is_dir():
        raise InstantiateError(f"{name} path '{path}' isn't a directory")


def _check_if_file_exists(name: str, path: Path):
    """
    Check if the given file exist

    :param name: Name of the file
    :param path: Location of the file
    :return:
    """
    if not path.exists():
        raise InstantiateError(f"{name} file '{path}' doesn't exists")
    elif not path.is_file():
        raise InstantiateError(f"{name} path '{path}' isn't a file")


def _copy_all(template_dir: Path, instance_dir: Path):
    """
    Copy all files and directories except from the description directory and the
    config.template.json file

    :param template_dir: The template directory as source
    :param instance_dir: The instance directory as destination
    :return:
    """

    for path in template_dir.iterdir():
        if path.name in ("config.template.json", "description"):
            continue
        elif path.is_dir():
            shutil.copytree(path, instance_dir / path.name)
        else:
            shutil.copy2(path, instance_dir)


def _filter_valid_languages(languages: List[str], test_suite: Suite) -> List[str]:
    """
    Filter out all languages for which the test suite isn't supported

    :param languages: languages to check
    :param test_suite: test suite to support
    :return: all given languages which support the test suite
    """

    def is_supported(language: str) -> bool:
        # TODO: get rid of the None
        language: Language = get_language(None, language)

        from tested.features import TypeSupport

        required = test_suite.get_used_features()

        # Check constructs
        available_constructs = language.supported_constructs()
        if not (required.constructs <= available_constructs):
            return False

        mapping = language.type_support_map()
        for t in required.types:
            if mapping[t] == TypeSupport.UNSUPPORTED:
                return False

        # Check language-specific evaluators
        for testcase in (
            testcase
            for tab in test_suite.tabs
            for context in tab.contexts
            for testcase in context.all_testcases()
        ):
            eval_langs = testcase.output.get_specific_eval_languages()
            if eval_langs is not None and language not in eval_langs:
                return False

        nested_types = filter(
            lambda x: x[0] in (BasicSequenceTypes.SET, BasicObjectTypes.MAP),
            required.nested_types,
        )
        restricted = language.restriction_map()
        for key, value_types in nested_types:
            if not (value_types <= restricted[key]):
                return False

        return True

    return list(filter(is_supported, languages))


def _get_config(config_json_path: Path) -> Dict[str, Any]:
    """
    Load exercise configuration

    :param config_json_path: Configuration file location
    :return: The loaded configuration dictionary
    """
    # noinspection PyTypeChecker
    with open(config_json_path, "r") as json_fd:
        return json.load(json_fd)


def _instantiate(
    template_dir: Path,
    instance_dir: Path,
    test_suite: Suite,
    descriptions: List[DescriptionFile],
    other_files_descriptions: List[Path],
    config_json_dict: Dict[str, Any],
    language: str,
    human_readable: bool = False,
    backup_descriptions: bool = False,
):
    """
    Instantiate template for a specific programming language

    :param template_dir: The template directory
    :param instance_dir: The instance directory
    :param test_suite: The test suite to use
    :param descriptions: The description file list
    :param other_files_descriptions: The other files from the description folders
    :param config_json_dict: Configuration dictionary
    :param language: The programming language
    :param human_readable: If the converted test suite must be human-readable
    :param backup_descriptions: Keep the old description folder
    :return:
    """
    config_dict = deepcopy(config_json_dict)
    config_json_file = instance_dir / "config.json"
    existing: bool
    if instance_dir.exists():
        if not instance_dir.is_dir():
            print(
                f"{instance_dir} is not a directory, instantiating {language} "
                f"failed!",
                file=sys.stderr,
            )
        if config_json_file.exists() and config_json_file.is_file():
            config = _get_config(config_json_file)
            try:
                dodona_internals = "internals"
                config_dict[dodona_internals] = config[dodona_internals]
            except KeyError:
                pass
        _remove_existing(instance_dir, backup_descriptions)
    else:
        instance_dir.mkdir(parents=True)
    # Copy all except descriptions
    _copy_all(template_dir, instance_dir)
    # Check test suite
    suite_file = template_dir / "evaluation" / config_dict["evaluation"]["test_suite"]
    if suite_file.suffix.lower() in (".yml", ".yaml"):
        suite_file_new = suite_file.with_suffix(f"{suite_file.suffix}.json")
        suite_file_new = instance_dir / "evaluation" / suite_file_new.name
        config_dict["evaluation"]["test_suite"] = suite_file_new.name
        with open(suite_file_new, "w") as fd:
            json.dump(
                test_suite,
                fd,
                default=pydantic_encoder,
                indent=2 if human_readable else None,
            )
    # Copy or generate descriptions
    _instantiate_descriptions(
        instance_dir, descriptions, other_files_descriptions, test_suite, language
    )
    # Prepare configuration
    config_dict["programming_language"] = language
    try:
        for i18n in config_dict["description"]["names"]:
            name = config_dict["description"]["names"][i18n]
            config_dict["description"]["names"][i18n] = f"{name} ({language})"
    except KeyError:
        pass
    with open(config_json_file, "w") as fd:
        json.dump(config_dict, fd, indent=2)


def _instantiate_descriptions(
    instance_dir: Path,
    descriptions: List[DescriptionFile],
    other_files_descriptions: List[Path],
    test_suite: Suite,
    language: str,
):
    """
    Instantiate description directory

    :param instance_dir: The instance directory
    :param descriptions: The description files to use
    :param other_files_descriptions: All other files and directories to copy
    :param test_suite: The test suite to determine the namespace
    :param language: Programming language
    :return:
    """
    description_dir = instance_dir / "description"
    description_dir.mkdir()
    # Copy the other files
    for path in other_files_descriptions:
        if path.is_dir():
            shutil.copytree(path, description_dir / path.name)
        else:
            shutil.copy2(path, description_dir)
    # Copy or generate descriptions
    for description in descriptions:
        # Prepare output file location
        if description.is_natural_language_explicit:
            file_name = (
                f"description.{description.natural_language}." f"{description.type}"
            )
            output_file = description_dir / file_name
        else:
            output_file = description_dir / f"description.{description.type}"
        # Copy or generate
        if description.is_template:
            # Generate
            instance = create_description_instance_from_template(
                description.template,
                language,
                description.natural_language,
                test_suite.namespace,
                description.type == "html",
            )
            with open(output_file, "w") as fd:
                print(instance, file=fd)
        else:
            # Copy files
            shutil.copy2(description.location, output_file)


def _parser_instance() -> ArgumentParser:
    """
    Get argument parser

    :return: the prepared argument parser
    """
    info = (
        "Included - Excluded = Programming languages that are candidates to "
        "generate instances for"
    )

    parser = ArgumentParser(
        description="Script for instantiating all supported languages"
    )
    parser.add_argument(
        "-i",
        "--programming_languages_included",
        type=str,
        nargs="+",
        help="Included programming languages to create instances "
        "for (default: all supported languages of "
        f"TESTed)\n{info}",
        default=[lang for lang in LANGUAGES if lang != "runhaskell"],
    )

    parser.add_argument(
        "-e",
        "--programming_languages_excluded",
        type=str,
        nargs="*",
        help="Excluded programming languages to create instances "
        f"for (default: nothing)\n{info}",
        default=[],
    )
    parser.add_argument(
        "-n",
        "--i18n",
        type=str,
        help="Natural language for descriptions if it can't be "
        "derived from the filename (options: 'en' or 'nl', "
        "default: 'en')",
        default="en",
    )
    parser.add_argument(
        "-H",
        "--human_readable",
        action="store_true",
        help="Generated test_suite in human readable format",
    )
    parser.add_argument(
        "-b",
        "--backup_descriptions",
        action="store_true",
        help="Keep old descriptions (with '.bak' extension)",
    )
    parser.add_argument("template_dir", type=str, help="Template directory")
    parser.add_argument("instances_dir", type=str, help="Instances directory")
    return parser


def _prepare_templates(descriptions: List[DescriptionFile]):
    """
    Prepare all description templates

    :param descriptions: list of the description files
    :return:
    """
    for description in descriptions:
        if not description.is_template:
            continue
        with open(description.location, "r") as file:
            description.template = prepare_template(
                file.read(), is_html=description.type == "html"
            )


def _read_test_suite(config_dict: Dict[str, Any], evaluation_dir: Path) -> Suite:
    """
    Read test suite from JSON or YAML.

    :param config_dict: Configuration information
    :param evaluation_dir: Directory containing the test suite
    :return: The test suite
    """

    try:
        suite_file = evaluation_dir / config_dict["evaluation"]["test_suite"]
    except KeyError:
        print(
            f"Not test suite given in the template configuration file", file=sys.stderr
        )
        sys.exit(6)
    _check_if_file_exists("Test suite", suite_file)

    with open(suite_file, "r") as file:
        loaded_suite = file.read()

    suffix = suite_file.suffixes[-1].lower()
    if suffix in (".yml", ".yaml"):
        return parse_dsl(loaded_suite)
    return parse_test_suite(loaded_suite)


def _remove_existing(instance_dir: Path, backup_descriptions: bool = False):
    """
    Remove the existing content of the instance directory

    :param instance_dir: The instance directory
    :param backup_descriptions: Keep the old descriptions directory or not
    :return:
    """
    updated = False
    for path in instance_dir.iterdir():
        if updated and path.name == "description.bak":
            continue
        elif backup_descriptions and path.name == "description":
            new_name = path.with_name("description.bak")
            if new_name.exists():
                if path.is_dir():
                    shutil.rmtree(new_name)
                else:
                    new_name.unlink()
            path.rename(path.with_name("description.bak"))
            updated = True
        elif path.is_dir():
            shutil.rmtree(path)
        else:
            path.unlink()


def _select_descriptions(descriptions: List[DescriptionFile]) -> List[DescriptionFile]:
    """
    Select best suited descriptions, templates and prefer markdown before html

    :param descriptions: list of all descriptions files
    :return: description files to use
    """

    def group_key(file: DescriptionFile) -> Tuple[bool, str]:
        return file.is_natural_language_explicit, file.natural_language

    selected = []
    descriptions.sort(key=group_key)
    natural_languages_groups = groupby(descriptions, key=group_key)
    for _, data_list in natural_languages_groups:
        # Select one template first templates and markdown before html
        selected.append(
            next(
                iter(
                    sorted(
                        data_list, key=lambda x: (x.is_template, x.type), reverse=True
                    )
                )
            )
        )
    return selected


def instantiate(
    template_dir: Path,
    instances_dir: Path,
    programming_languages: Optional[List[str]] = None,
    default_i18n: str = "en",
    human_readable: bool = False,
    backup_descriptions: bool = False,
):
    """
    Instantiate a template exercise for all the supported programming language in
    the given list of programming languages

    :param template_dir: The template exercise directory
    :param instances_dir: The instances directory
    :param programming_languages: An optional list of possible programming language,
    if no list given al languages from TESTed
    :param default_i18n: The default language for the description files without
    language
    :param human_readable: Generated JSON test suite must be human-readable
    :param backup_descriptions: Keep the old description folder
    :return:
    """

    if programming_languages is None:
        programming_languages = [lang for lang in LANGUAGES if lang != "runhaskell"]

    evaluation_dir = template_dir / "evaluation"
    description_dir = template_dir / "description"
    config_template = template_dir / "config.template.json"

    _check_if_all_languages_exists(programming_languages)

    _check_if_directory_exists("Template", template_dir)
    _check_if_directory_exists("Instances", instances_dir)
    _check_if_directory_exists("Evaluation", evaluation_dir)
    _check_if_directory_exists("Description", description_dir)

    _check_if_file_exists("Template config", config_template)

    template_config_dict = _get_config(config_template)
    suite = _read_test_suite(template_config_dict, evaluation_dir)
    descriptions_files, other_files_description = _analyse_description_dir(
        description_dir=description_dir, default_i18n=default_i18n
    )
    descriptions_files = _select_descriptions(descriptions_files)
    _prepare_templates(descriptions_files)
    programming_languages = _filter_valid_languages(programming_languages, suite)
    for language in programming_languages:
        _instantiate(
            template_dir=template_dir,
            instance_dir=instances_dir / language,
            test_suite=suite,
            descriptions=descriptions_files,
            other_files_descriptions=other_files_description,
            config_json_dict=template_config_dict,
            language=language,
            human_readable=human_readable,
            backup_descriptions=backup_descriptions,
        )


if __name__ == "__main__":
    args = _parser_instance().parse_args()
    temp_dir, inst_dir = Path(args.template_dir), Path(args.instances_dir)

    prog_langs = list(
        sorted(
            set(args.programming_languages_included)
            - set(args.programming_languages_excluded)
        )
    )

    try:
        instantiate(
            temp_dir,
            inst_dir,
            prog_langs,
            args.i18n,
            args.human_readable,
            args.backup_descriptions,
        )
    except InstantiateError as e:
        print(e, file=sys.stderr)
        sys.exit(-1)
