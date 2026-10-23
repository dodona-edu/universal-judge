"""
Tests for the source rewrites in tested.languages.treesitter.
"""

import pytest

from tested.languages.treesitter import (
    haskell_header_line,
    rewrite_c_main,
    rewrite_kotlin_main,
)

C_PARAMETERS = {"c": "int argc, char** argv", "cpp": "int argc, char* argv[]"}


@pytest.mark.parametrize("language", ["c", "cpp"])
@pytest.mark.parametrize(
    "source,expected",
    [
        pytest.param(
            '#include <stdio.h>\n\nint main() {\n    puts("x");\n}\n',
            '#include <stdio.h>\n\nint solution_main(ARGS) {\n    puts("x");\nreturn 0;}\n',
            id="main-last",
        ),
        pytest.param(
            "int main() {\n    helper();\n}\n\nvoid helper(void) {\n}\n",
            "int solution_main(ARGS) {\n    helper();\nreturn 0;}\n\n"
            "void helper(void) {\n}\n",
            id="void-helper-after-main",
        ),
        pytest.param(
            "int main() {\n    return helper();\n}\n\nint helper(void) {\n}\n",
            "int solution_main(ARGS) {\n    return helper();\nreturn 0;}\n\n"
            "int helper(void) {\n}\n",
            id="int-helper-after-main",
        ),
        pytest.param(
            'int main() {\n    puts("}");\n}\n',
            'int solution_main(ARGS) {\n    puts("}");\nreturn 0;}\n',
            id="brace-in-string",
        ),
        pytest.param(
            "int main() {\n    // }\n}\n",
            "int solution_main(ARGS) {\n    // }\nreturn 0;}\n",
            id="brace-in-comment",
        ),
        pytest.param(
            "int main() {\n    putchar('}');\n}\n",
            "int solution_main(ARGS) {\n    putchar('}');\nreturn 0;}\n",
            id="brace-in-char",
        ),
        pytest.param(
            "int main() {\n}\n\nstruct point {\n    int x;\n};\n",
            "int solution_main(ARGS) {\nreturn 0;}\n\nstruct point {\n    int x;\n};\n",
            id="struct-after-main",
        ),
        pytest.param(
            "int main() {\n    if (x) {\n        a();\n    } else {\n        b();\n    }\n}\n",
            "int solution_main(ARGS) {\n    if (x) {\n        a();\n    } else {\n"
            "        b();\n    }\nreturn 0;}\n",
            id="if-else-at-end",
        ),
        pytest.param(
            "int main() {\n    exit(0);\n}\n",
            "int solution_main(ARGS) {\n    exit(0);\nreturn 0;}\n",
            id="exit-at-end",
        ),
        pytest.param(
            "int main() {\n    while (1) {\n    }\n}\n",
            "int solution_main(ARGS) {\n    while (1) {\n    }\nreturn 0;}\n",
            id="infinite-loop",
        ),
        pytest.param(
            'int\tmain()\t{\tputs("x");\t}\n',
            'int\tsolution_main(ARGS)\t{\tputs("x");\treturn 0;}\n',
            id="tab-one-liner",
        ),
        pytest.param(
            '// int main() {\nconst char* s = "int main() {";\nint main() {\n}\n',
            '// int main() {\nconst char* s = "int main() {";\n'
            "int solution_main(ARGS) {\nreturn 0;}\n",
            id="main-in-comment-and-string",
        ),
        pytest.param(
            "int main();\n\nint main() {\n}\n",
            "int solution_main(ARGS);\n\nint solution_main(ARGS) {\nreturn 0;}\n",
            id="prototype",
        ),
        pytest.param(
            "int main(const int argc, char** argv) {\n}\n",
            "int solution_main(const int argc, char** argv) {\nreturn 0;}\n",
            id="const-argc",
        ),
        pytest.param(
            "int main() /* c */ {\n}\n",
            "int solution_main(ARGS) /* c */ {\nreturn 0;}\n",
            id="comment-before-brace",
        ),
        pytest.param(
            "int main(void) {\n}\n",
            "int solution_main(ARGS) {\nreturn 0;}\n",
            id="main-void",
        ),
        pytest.param(
            "void main() {\n}\n",
            "int solution_main(ARGS) {\nreturn 0;}\n",
            id="void-main",
        ),
        pytest.param(
            "int main(\n) {\n}\n",
            "int solution_main(ARGS\n) {\nreturn 0;}\n",
            id="multiline-parameters",
        ),
        pytest.param(
            "#ifdef X\nint main() {\n#else\nint main(void) {\n#endif\n}\n",
            "#ifdef X\nint main() {\n#else\nint solution_main(ARGS) {\n#endif\nreturn 0;}\n",
            id="ifdef-branches",
        ),
        pytest.param(
            "int main() {\n    int x = ;\n}\n",
            "int solution_main(ARGS) {\n    int x = ;\n}\n",
            id="syntax-error-in-body-renamed",
        ),
        pytest.param(
            "int main(int argc, {\n}\n",
            "int main(int argc, {\n}\n",
            id="syntax-error-in-parameters-unchanged",
        ),
        pytest.param(
            "#define END }\n\nint main() {\nEND\n",
            "#define END }\n\nint main() {\nEND\n",
            id="macro-closed-main-unchanged",
        ),
        pytest.param(
            "int helper(void) {\n    return 1;\n}\n",
            "int helper(void) {\n    return 1;\n}\n",
            id="no-main-unchanged",
        ),
    ],
)
def test_c_cpp_rewrite_main(language: str, source: str, expected: str):
    expected = expected.replace("ARGS", C_PARAMETERS[language])
    assert rewrite_c_main(language, source) == expected


@pytest.mark.parametrize(
    "language,source,expected",
    [
        pytest.param(
            "c",
            "#if 0\nint main() {\n#endif\nint main() {\n}\n",
            "#if 0\nint main() {\n#endif\nint solution_main(int argc, char** argv) {\n"
            "return 0;}\n",
            id="c-if-0-unbalanced-brace",
        ),
        pytest.param(
            "cpp",
            "#if 0\nint main() {\n#endif\nint main() {\n}\n",
            "#if 0\nint main() {\n#endif\nint main() {\n}\n",
            id="cpp-if-0-unbalanced-brace-unchanged",
        ),
        pytest.param(
            "cpp",
            "auto main() -> int {\n}\n",
            "auto solution_main(int argc, char* argv[]) -> int {\nreturn 0;}\n",
            id="cpp-trailing-return-type",
        ),
        pytest.param(
            "cpp",
            "int main() try {\n    f();\n} catch (int e) {\n    g();\n} catch (...) {\n}\n",
            "int solution_main(int argc, char* argv[]) try {\n    f();\nreturn 0;}"
            " catch (int e) {\n    g();\nreturn 0;} catch (...) {\nreturn 0;}\n",
            id="cpp-function-try-block",
        ),
    ],
)
def test_c_cpp_rewrite_main_language_specific(
    language: str, source: str, expected: str
):
    assert rewrite_c_main(language, source) == expected


@pytest.mark.kotlin
@pytest.mark.parametrize(
    "source,expected",
    [
        pytest.param(
            "fun main() {\n}\n",
            "fun solutionMain(args: Array<String> = emptyArray()) {\n}\n",
            id="no-parameters",
        ),
        pytest.param(
            "fun main(input: Array<String>) {\n}\n",
            "fun solutionMain(input: Array<String>) {\n}\n",
            id="array-parameter",
        ),
        pytest.param(
            "fun main(vararg args: String) {\n}\n",
            "fun solutionMain(args: Array<String>) {\n}\n",
            id="vararg-parameter",
        ),
        pytest.param(
            "// fun main() {\nfun main(\n) {\n}\n",
            "// fun main() {\n"
            "fun solutionMain(args: Array<String> = emptyArray()\n) {\n}\n",
            id="main-in-comment",
        ),
        pytest.param(
            "class A {\n    fun main() {}\n}\nfun main() = println(1)\n",
            "class A {\n    fun main() {}\n}\n"
            "fun solutionMain(args: Array<String> = emptyArray()) = println(1)\n",
            id="member-main-unchanged",
        ),
        pytest.param(
            "fun main() {\n    println(\n}\n",
            "fun solutionMain(args: Array<String> = emptyArray()) {\n    println(\n}\n",
            id="syntax-error-in-body-renamed",
        ),
        pytest.param(
            "fun main(args: Array<String>, {\n}\n",
            "fun main(args: Array<String>, {\n}\n",
            id="syntax-error-in-parameters-unchanged",
        ),
    ],
)
def test_kotlin_rewrite_main(source: str, expected: str):
    assert rewrite_kotlin_main(source) == expected


@pytest.mark.haskell
@pytest.mark.parametrize(
    "source,expected",
    [
        pytest.param("main = pure ()\n", 0, id="no-pragma"),
        pytest.param("{-# LANGUAGE LambdaCase #-}\nmain = pure ()\n", 1, id="pragma"),
        pytest.param(
            "-- Greeting\n{-# LANGUAGE LambdaCase #-}\nmain = pure ()\n",
            2,
            id="comment-before-pragma",
        ),
        pytest.param(
            "{-# LANGUAGE LambdaCase,\n             TupleSections #-}\nmain = pure ()\n",
            2,
            id="two-line-pragma",
        ),
        pytest.param("{-# LANGUAGE LambdaCase #-}", 1, id="pragma-only"),
        pytest.param("module Submission where\nmain = pure ()\n", None, id="header"),
        pytest.param("main = (pure ()\n", None, id="syntax-error"),
    ],
)
def test_haskell_header_line(source: str, expected: int | None):
    assert haskell_header_line(source) == expected
