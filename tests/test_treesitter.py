"""
Tests for the source rewrites in tested.languages.treesitter.
"""

import pytest

from tested.languages.treesitter import haskell_header_line, rewrite_kotlin_main


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
