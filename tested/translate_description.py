import sys

from argparse import ArgumentParser, FileType
from functools import partial
from mako.template import Template
from tested.utils import smart_close
from tested.languages import get_language, language_exists

if __name__ == "__main__":
    parser = ArgumentParser(
        description="Translate description for language"
    )
    parser.add_argument('-d', '--description', type=FileType('r'),
                        help="Description", default="-")
    parser.add_argument('-l', '--language', type=str, help="Language",
                        default="python")
    parser.add_argument('-o', '--output', type=FileType('w'),
                        help="Output description instance", default="-")
    parser.add_argument('-i', '--i18n', type=str,
                        help="Natural language (en, nl), default: nl", default="nl")
    type_group = parser.add_mutually_exclusive_group()
    type_group.add_argument('-M', '--markdown', action='store_true',
                            help="Generate markdown")
    type_group.add_argument('-H', '--html', action='store_true',
                            help="Generate html")

    parser = parser.parse_args()
    with smart_close(parser.description) as template_fd:
        template = Template(template_fd.read(), cache_enabled=False)

    if not language_exists(parser.language):
        print(f"Language {parser.language} doesn't exists", file=sys.stderr)
        sys.exit(-1)

    language = get_language(parser.language)

    is_html = parser.html

    with smart_close(parser.output) as output_fd:
        print(template.render(
            get_appendix=partial(language.get_appendix, is_html=is_html),
            get_function_name=language.get_function_name,
            get_type_name=language.get_type_name,
            get_code_start=partial(language.get_code_start, is_html=is_html),
            get_code_end=partial(language.get_code_end, is_html=is_html),
            get_code=partial(language.get_code, is_html=is_html),
        ), file=output_fd)
