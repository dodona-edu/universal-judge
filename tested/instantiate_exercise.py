import json
import sys
from argparse import ArgumentParser
from dataclasses import dataclass
from itertools import groupby
from pathlib import Path
from typing import List, Any, Dict, Tuple, Optional

from mako.template import Template

from tested.description_instance import prepare_template
from tested.dsl import SchemaParser
from tested.languages import LANGUAGES, language_exists, Language, get_language
from tested.testplan import Plan, _PlanModel

config_json = "config.json"


@dataclass
class DescriptionFile:
    location: Path
    type: str = "md"
    natural_language: str = "en"
    is_natural_language_explicit: bool = False
    is_template: bool = False
    template: Optional[Template] = None


def _analyse_description_dir(description_dir: Path,
                             default_i18n: str
                             ) -> Tuple[List[DescriptionFile], List[Path]]:
    """
    Read the description directory

    :param description_dir: Description directory to analyse
    :return: tuple of a list of description files and a list of the other files
    """
    descriptions, other = [], []
    for path in description_dir.iterdir():
        if (path.is_file() and path.name.startswith("description") and
                path.suffix.lower() in (".md", ".html", ".mako")):
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


def _check_if_all_languages_exists_or_exit(languages: List[str]):
    for language in languages:
        if not language_exists(language):
            print(f"Programming language '{language}' isn't supported by TESTed",
                  file=sys.stderr)
            sys.exit(3)


def _check_if_directory_exists_or_exit(name: str, path: Path):
    if not path.exists():
        print(f"{name} directory '{path}' doesn't exists", file=sys.stderr)
        sys.exit(1)
    elif not path.is_dir():
        print(f"{name} path '{path}' isn't a directory", file=sys.stderr)
        sys.exit(2)


def _check_if_file_exists_or_exit(name: str, path: Path):
    if not path.exists():
        print(f"{name} file '{path}' doesn't exists", file=sys.stderr)
        sys.exit(1)
    elif not path.is_file():
        print(f"{name} path '{path}' isn't a file", file=sys.stderr)
        sys.exit(5)


def _filter_valid_languages(languages: List[str], testplan: Plan) -> List[str]:
    """
    Filter out all languages for which the testplan isn't supported
    :param languages: languages to check
    :param testplan: testplan to support
    :return: all given languages which support the testplan
    """

    def is_supported(language: str) -> bool:
        language: Language = get_language(language)

        from .languages.config import TypeSupport

        required = testplan.get_used_features()

        # Check constructs
        available_constructs = language.supported_constructs()
        if not (required.constructs <= available_constructs):
            return False

        mapping = language.type_support_map()
        for t in required.types:
            if mapping[t] == TypeSupport.UNSUPPORTED:
                return False

        # Check language specific evaluators
        for testcase in (testcase for tab in testplan.tabs
                         for run in tab.runs
                         for context in run.contexts
                         for testcase in context.all_testcases()):
            eval_langs = testcase.output.get_specific_eval_languages()
            if eval_langs is not None and language not in eval_langs:
                return False
        return True

    return list(filter(is_supported, languages))


def _get_template_config(config_template_json: Path) -> Dict[str, Any]:
    with open(config_template_json, 'r') as json_fd:
        return json.load(json_fd)


def _parser_instance() -> ArgumentParser:
    info = "Included - Excluded = Programming languages that are candidates to " \
           "generate instances for"

    parser = ArgumentParser(
        description="Script for instantiating all supported languages"
    )
    parser.add_argument("-i", "--programming_languages_included", type=str,
                        nargs='+',
                        help="Included programming languages to create instances "
                             "for (default: all supported languages of "
                             f"TESTed)\n{info}",
                        default=[lang for lang in LANGUAGES if
                                 lang != "runhaskell"])

    parser.add_argument("-e", "--programming_languages_excluded", type=str,
                        nargs='*',
                        help="Excluded programming languages to create instances "
                             f"for (default: nothing)\n{info}",
                        default=[])
    parser.add_argument("-n", "--i18n", type=str,
                        help="Natural language for descriptions if it can't be "
                             "derived from the filename (options: 'en' or 'nl', "
                             "default: 'en')",
                        default="en")
    parser.add_argument("-b", "--backup", action='store_true',
                        help="Keep old descriptions (with '.bak' extension)")
    parser.add_argument("template_dir", type=str, help="Template directory")
    parser.add_argument("instances_dir", type=str, help="Instances directory")
    return parser


def _prepare_templates(descriptions: List[DescriptionFile]):
    for description in descriptions:
        if not description.is_template:
            continue
        with open(description.location, 'r') as file:
            description.template = prepare_template(
                file.read(), is_html=description.type == "html"
            )


def _read_plan(config_dict: Dict[str, Any], evaluation_dir: Path) -> Plan:
    try:
        plan_file = evaluation_dir / config_dict["evaluation"]["plan_name"]
    except KeyError:
        print(f"Not testplan given in the template configuration file",
              file=sys.stderr)
        sys.exit(6)
    _check_if_file_exists_or_exit("Testplan", plan_file)

    with open(plan_file, 'r') as file:
        loaded_plan = file.read()

    suffix = plan_file.suffixes[-1].lower()
    if suffix in ('.yml', '.yaml'):
        schema_parser = SchemaParser()
        return schema_parser.load_str(loaded_plan)
    return _PlanModel.parse_raw(loaded_plan).__root__


def _select_descriptions(descriptions: List[DescriptionFile]
                         ) -> List[DescriptionFile]:
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
        selected.append(next(iter(
            sorted(data_list, key=lambda x: (x.is_template, x.type), reverse=True)
        )))
    return selected


if __name__ == "__main__":
    args = _parser_instance().parse_args()
    temp_dir, inst_dir = Path(args.template_dir), Path(args.instances_dir)

    eval_dir = temp_dir / "evaluation"
    desc_dir = temp_dir / "description"
    config_template = temp_dir / "config.template.json"

    prog_langs = list(set(args.programming_languages_included) -
                      set(args.programming_languages_excluded))

    _check_if_all_languages_exists_or_exit(prog_langs)

    _check_if_directory_exists_or_exit("Template", temp_dir)
    _check_if_directory_exists_or_exit("Instances", inst_dir)
    _check_if_directory_exists_or_exit("Evaluation", eval_dir)
    _check_if_directory_exists_or_exit("Description", desc_dir)

    _check_if_file_exists_or_exit("Template config", config_template)

    other_files = [
        path for path in temp_dir.iterdir()
        if path not in (eval_dir, desc_dir, config_template)
    ]

    template_config_dict = _get_template_config(config_template)
    plan = _read_plan(template_config_dict, eval_dir)
    descriptions_files, other_files_description = _analyse_description_dir(
        description_dir=desc_dir, default_i18n=args.i18n
    )
    descriptions_files = _select_descriptions(descriptions_files)
    _prepare_templates(descriptions_files)
    prog_langs = _filter_valid_languages(prog_langs, plan)
    pass
