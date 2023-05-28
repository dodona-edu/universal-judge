from pathlib import Path

import pytest

from tested.configs import DodonaConfig, GlobalConfig
from tested.languages import LANGUAGES, Language

# noinspection PyProtectedMember
from tested.languages.utils import _replace_code_line_number
from tested.testsuite import Suite


def get_language(workdir: Path, language: str) -> Language:
    dodona_config = DodonaConfig(
        resources="",
        source="",
        time_limit=0,
        memory_limit=0,
        natural_language="nl",
        programming_language=language,
        workdir=workdir,
        judge="",
    )
    global_config = GlobalConfig(
        dodona=dodona_config,
        testcase_separator_secret="",
        context_separator_secret="",
        suite=Suite(tabs=[]),
    )
    return LANGUAGES[language](global_config)


def test_javascript_assertion_error():
    workdir = "/home/bliep/bloep/universal-judge/workdir"
    language_config = get_language(workdir, "javascript")
    original = f"""AssertionError [ERR_ASSERTION]: ongeldig bericht
    at bigram2letter ({workdir}/execution00/submission.js:86:13)
    at {workdir}/execution00/submission.js:98:32
    at Array.map (<anonymous>)
    at Codeersleutel.decodeer ({workdir}/execution00/submission.js:98:18)
    at context0 ({workdir}/execution00/execution00.js:78:31)
    at async {workdir}/execution00/execution00.js:1515:13
"""
    expected_cleaned = f"""AssertionError [ERR_ASSERTION]: ongeldig bericht
    at bigram2letter (<code>:86:13)
    at <code>:98:32
    at Array.map (<anonymous>)
    at Codeersleutel.decodeer (<code>:98:18)
"""
    actual_cleaned = language_config.cleanup_stacktrace(original)
    assert actual_cleaned == expected_cleaned


def test_javascript_type_error():
    workdir = "/home/bliep/bloep/universal-judge/workdir"
    language_config = get_language(workdir, "javascript")
    original = f"""TypeError: submission.Codeersleutel is not a constructor
    at context0 ({workdir}/execution00/execution00.js:46:17)
    at {workdir}/execution00.js:1515:19
    at Object.<anonymous> ({workdir}/execution00/execution00.js:1573:7)
    at Module._compile (node:internal/modules/cjs/loader:1254:14)
    at Module._extensions..js (node:internal/modules/cjs/loader:1308:10)
    at Module.load (node:internal/modules/cjs/loader:1117:32)
    at Module._load (node:internal/modules/cjs/loader:958:12)
    at Function.executeUserEntryPoint [as runMain] (node:internal/modules/run_main:81:12)
    at node:internal/main/run_main_module:23:47
    """
    expected_cleaned = f"TypeError: Codeersleutel is not a constructor\n"
    actual_cleaned = language_config.cleanup_stacktrace(original)
    assert actual_cleaned == expected_cleaned


@pytest.mark.parametrize("language", LANGUAGES.keys())
def test_empty_stacktrace(language):
    workdir = "/home/bliep/bloep/universal-judge/workdir"
    language_config = get_language(workdir, language)
    original = ""
    expected_cleaned = ""
    actual_cleaned = language_config.cleanup_stacktrace(original)
    assert actual_cleaned == expected_cleaned


def test_javascript_compilation_error():
    workdir = "/home/bliep/bloep/universal-judge/workdir"
    language_config = get_language(workdir, "javascript")
    original = f"""
    {workdir}/common/submission.js:4
    const deepcopy = cellen => cellen.map(([x, y]) => [x, y]));
                                                             ^
    
    SyntaxError: Unexpected token ')'
        at internalCompileFunction (node:internal/vm:73:18)
        at wrapSafe (node:internal/modules/cjs/loader:1176:20)
        at checkSyntax (node:internal/main/check_syntax:67:3)
    
    Node.js v18.15.0
    """
    expected_cleaned = """
    <code>:4
    const deepcopy = cellen => cellen.map(([x, y]) => [x, y]));
                                                             ^
    
    SyntaxError: Unexpected token ')'
        at internalCompileFunction (node:internal/vm:73:18)
        at wrapSafe (node:internal/modules/cjs/loader:1176:20)
        at checkSyntax (node:internal/main/check_syntax:67:3)
    
    Node.js v18.15.0
    """
    actual_cleaned = language_config.cleanup_stacktrace(original)
    assert actual_cleaned == expected_cleaned


def test_python_compilation_error():
    language_config = get_language("/temp", "python")
    original = """*** Error compiling './submission.py'...
    Sorry: IndentationError: unexpected indent (submission.py, line 9)
"""
    expected = """*** Error compiling '<code>'...
    Sorry: IndentationError: unexpected indent (<code>:9)
"""
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_python_exception():
    language_config = get_language("/temp", "python")
    original = """NameError: name 'does_not_exist' is not defined
      File "./execution_0_0.py", line 48, in execution_0_0_context_0
      File "./submission.py", line 1, in <module>
"""
    expected = """NameError: name 'does_not_exist' is not defined
      File "<code>:1"
"""
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_csharp_exception():
    workdir = "/home/bliep/bloep/universal-judge/workdir"
    language_config = get_language(workdir, "csharp")
    original = f"""at Submission.Main(String[] args) in {workdir}/common/Submission.cs:line 7
    at Tested.Execution00.Context0() in {workdir}/common/Execution00.cs:line 67'
    """
    expected = "at Submission.Main(String[] args) in <code>:7\n"
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_csharp_compilation():
    workdir = "/home/bliep/bloep/universal-judge/workdir"
    language_config = get_language(workdir, "csharp")
    original = f"""Determining projects to restore...
    Restored {workdir}/common/dotnet.csproj (in 81 ms).
    {workdir}/common/Submission.cs(13,20): error CS1002: ; expected [{workdir}/common/dotnet.csproj]
    {workdir}/common/Submission.cs(13,26): error CS1002: ; expected [{workdir}/common/dotnet.csproj]
    {workdir}/common/Submission.cs(13,30): error CS1001: Identifier expected [{workdir}/common/dotnet.csproj]
    {workdir}/common/Submission.cs(13,30): error CS1002: ; expected [{workdir}/common/dotnet.csproj]
    {workdir}/common/Submission.cs(13,34): error CS1002: ; expected [{workdir}/common/dotnet.csproj]
    """
    expected = f"""    <code>:13:20: error CS1002: ; expected
    <code>:13:26: error CS1002: ; expected
    <code>:13:30: error CS1001: Identifier expected
    <code>:13:30: error CS1002: ; expected
    <code>:13:34: error CS1002: ; expected
    """
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_java_compilation_error():
    original = """Submission.java:1: error: class, interface, enum, or record expected
    mfzej àryhg çyh aiogharuio ghqgh
    ^
    1 error"""
    language_config = get_language("test", "java")
    expected = """<code>:1: error: class, interface, enum, or record expected
    mfzej àryhg çyh aiogharuio ghqgh
    ^
    1 error
"""
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_java_runtime_error():
    original = """java.lang.ClassCastException: class java.lang.Integer cannot be cast to class java.lang.String (java.lang.Integer and java.lang.String are in module java.base of loader 'bootstrap')
	at Submission.main(Submission.java:7)
	at Execution00.context0(Execution00.java:60)
	at Execution00.execute(Execution00.java:70)
	at Execution00.main(Execution00.java:81)
	at Selector.main(Selector.java:7)
    """
    language_config = get_language("test", "java")
    expected = """java.lang.ClassCastException: class java.lang.Integer cannot be cast to class java.lang.String (java.lang.Integer and java.lang.String are in module java.base of loader 'bootstrap')
	at Submission.main(<code>:7)
"""
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_bash_runtime_error():
    original = "submission.sh: line 1: d: opdracht niet gevonden\n"
    language_config = get_language("test", "bash")
    expected = "<code>:1: d: opdracht niet gevonden\n"
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_bash_compilation_error():
    original = """
    submission.sh: line 1: syntaxfout nabij onverwacht symbool '('
    submission.sh: line 1: `def isISBN10(code):'
    """
    language_config = get_language("test", "bash")
    expected = """
    <code>:1: syntaxfout nabij onverwacht symbool '('
    <code>:1: `def isISBN10(code):'
    """
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_c_compilation_error():
    original = """In file included from execution_0_0.c:6,
                     from selector.c:6:
    submission.c:3:1: fout: unknown type name ‘mfzej’
        3 | mfzej àryhg çyh aiogharuio ghqgh
          | ^~~~~
    submission.c:3:1
    ...
    _main(1, args);
          |             ^~~~~~~~~~~~~
    execution_0_0.c: In functie ‘execution_0_0’:
    execution_0_0.c:45:9: fout: ‘execution_0_0_value_file’ undeclared (first use in this function)
       45 |         execution_0_0_value_file = fopen("N62PYrpfo_values.txt", "w");
          |         ^~~~~~~~~~~~~~~~~~~~~~~~
"""
    language_config = get_language("test", "c")
    expected = """    <code>:3:1: fout: unknown type name ‘mfzej’
        3 | mfzej àryhg çyh aiogharuio ghqgh
          | ^~~~~
    <code>:3:1
"""
    actual = language_config.cleanup_stacktrace(original)
    assert actual == expected


def test_code_link_line_number_replacement_works(tmp_path: Path, pytestconfig):
    stacktrace = f"""AssertionError [ERR_ASSERTION]: ongeldig bericht
    at bigram2letter (<code>:86:13)
    at <code>:98:32
    at Array.map (<anonymous>)
    at Codeersleutel.decodeer (<code>:98:18)
    """
    expected = f"""AssertionError [ERR_ASSERTION]: ongeldig bericht
    at bigram2letter (<code>:91:13)
    at <code>:103:32
    at Array.map (<anonymous>)
    at Codeersleutel.decodeer (<code>:103:18)
    """
    actual = _replace_code_line_number(5, stacktrace)
    assert actual == expected
