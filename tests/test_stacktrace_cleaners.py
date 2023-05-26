from pathlib import Path

from tested.configs import DodonaConfig, GlobalConfig
from tested.languages import LANGUAGES, Language
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


def test_javascript_empty():
    workdir = "/home/bliep/bloep/universal-judge/workdir"
    language_config = get_language(workdir, "javascript")
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
