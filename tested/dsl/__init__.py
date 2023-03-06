# from .statement import Parser, ParseError
# from .translate_parser_old import SchemaParser as OldSchemaParser
# from .translate_parser_fast import SchemaParser as FastSchemaParser
from .translate_parser import parse_dsl, translate_to_testplan
from .ast_translator import parse_string
