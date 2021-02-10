import os
import sys

from argparse import ArgumentParser, FileType
from functools import partial
from mako.template import Template

from tested.configs import Bundle, DodonaConfig
from tested.testplan import Plan
from tested.utils import smart_close
from tested.languages import get_language, language_exists


def create_description_instance(mako_template: str,
                                programming_language: str = "python",
                                natural_language: str = "en",
                                namespace: str = "submission",
                                is_html: bool = True) -> str:
    if not language_exists(programming_language):
        raise ValueError(f"Language {programming_language} doesn't exists")
    template = Template(mako_template, cache_enabled=False)

    language = get_language(programming_language)

    bundle = Bundle(
        config=DodonaConfig(
            resources="",
            source="",
            time_limit=0,
            memory_limit=0,
            natural_language=natural_language,
            programming_language=programming_language,
            workdir="",
            judge=""
        ),
        out=open(os.devnull, 'w'),
        lang_config=language,
        secret="",
        plan=Plan(namespace=namespace)
    )

    return template.render(
        appendix=partial(language.get_appendix, bundle=bundle, is_html=is_html),
        function_name=language.get_function_name,
        type_name=partial(language.get_type_name, bundle=bundle, is_html=is_html),
        code_start=partial(language.get_code_start, is_html=is_html),
        code_end=partial(language.get_code_end, is_html=is_html),
        statement=partial(language.get_code, bundle=bundle, is_html=is_html,
                          statement=True),
        expression=partial(language.get_code, bundle=bundle, is_html=is_html,
                           statement=False),
    )


if __name__ == "__main__":
    parser = ArgumentParser(
        description="Translate description for language"
    )
    parser.add_argument('-d', '--description', type=FileType('r'),
                        help="Description", default="-")
    parser.add_argument('-l', '--language', type=str, help="Programming language",
                        default="python")
    parser.add_argument('-o', '--output', type=FileType('w'),
                        help="Output description instance", default="-")
    parser.add_argument('-i', '--i18n', type=str,
                        help="Natural language (en, nl), default: en", default="en")
    parser.add_argument('-n', '--namespace', type=str,
                        help="Namespace of the submission", default="submission")

    type_group = parser.add_mutually_exclusive_group()
    type_group.add_argument('-M', '--markdown', action='store_true',
                            help="Generate markdown")
    type_group.add_argument('-H', '--html', action='store_true',
                            help="Generate html (default)")

    parser = parser.parse_args()
    with smart_close(parser.description) as template_fd:
        template_str = template_fd.read()

    try:
        rendered = create_description_instance(template_str, parser.language,
                                               parser.i18n, parser.namespace,
                                               not bool(parser.markdown))
    except ValueError as e:
        print(e, file=sys.stderr)
        sys.exit(-1)

    with smart_close(parser.output) as output_fd:
        print(rendered, file=output_fd)
