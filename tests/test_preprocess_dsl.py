from pathlib import Path

from pytest_mock import MockerFixture

import tested
from tested.dsl.translate_parser import _parse_yaml, _validate_dsl
from tested.nat_translation import (
    convert_to_yaml,
    create_enviroment,
    parse_yaml,
    run_translation,
    translate_yaml,
    validate_pre_dsl,
)

test_unit_yaml_str = """
translations:
  animal:
    en: "animals"
    nl: "dieren"
  result:
    en: "results"
    nl: "resultaten"
  elf:
    en: "eleven"
    nl: "elf"
  select:
    en: "select"
    nl: "selecteer"
tabs:
  - tab: "{{ animal|braces }}_{{ '{' + result + '}' }}"
    translations:
      animal:
        en: "animal_tab"
        nl: "dier_tab"
    contexts:
      - testcases:
        - statement: !natural_language
            en: '{{result}} = Trying(10)'
            nl: '{{result}} = Proberen(10)'
        - expression: !natural_language
            en: 'count_words({{result}})'
            nl: 'tel_woorden({{result}})'
          return: !natural_language
            en: 'The {{result}} is 10'
            nl: 'Het {{result}} is 10'
        - expression: !natural_language
            en: "count"
            nl: "tellen"
          return: !natural_language
            en: !expression 'count'
            nl: !expression 'tellen'
        - expression: 'ok(10)'
          return: !oracle
            value: !natural_language
              en: "The {{result}} 10 is OK!"
              nl: "Het {{result}} 10 is OK!"
            oracle: "custom_check"
            file: "test.py"
            name: "evaluate_test"
            arguments: !natural_language
              en: ["The value", "is OK!", "is not OK!"]
              nl: ["Het {{result}}", "is OK!", "is niet OK!"]
          description: !natural_language
            en: "Ten"
            nl: "Tien"
        files: !natural_language
          en:
            - name: "file.txt"
              url: "media/workdir/file.txt"
          nl:
            - name: "fileNL.txt"
              url: "media/workdir/fileNL.txt"
        translations:
          result:
            en: "results_context"
            nl: "resultaten_context"
      - testcases:
          - statement: !natural_language
              en: 'result = Trying(11)'
              nl: 'resultaat = Proberen(11)'
          - expression: 'result'
            return: '11_{elf}'
            description:
              description: !natural_language
                en: "Eleven_{{elf}}"
                nl: "Elf_{{elf}}"
              format: "code"
  - tab: '{{animal}}'
    testcases:
      - expression: !natural_language
          en: "tests(11)"
          nl: "testen(11)"
        return: 11
      - expression: !programming_language
          javascript: "{{animal}}_javascript(1 + 1)"
          typescript: "{{animal}}_typescript(1 + 1)"
          java: "Submission.{{animal}}_java(1 + 1)"
          python: !natural_language
            en: "{{animal}}_python_en(1 + 1)"
            nl: "{{animal}}_python_nl(1 + 1)"
        return: 2
  - tab: 'test'
    testcases:
      - expression: "{{select}}('a', {'a': 1, 'b': 2})"
        return: 1
  - tab: "task"
    contexts: !natural_language
        en:
          - testcases:
            - statement: '{{result}} = Trying(10)'
            - expression: 'count_words({{result}})'
              return: 'The {{result}} is 10'
            - expression: "count"
              return: !expression 'count'
        nl:
          - testcases:
            - statement: '{{result}} = Proberen(10)'
            - expression: 'tel_woorden({{result}})'
              return: 'Het {{result}} is 10'
            - expression: "tellen"
              return: !expression 'tellen'
  - tab: "task2"
    contexts:
        - testcases: !natural_language
            en:
              - statement: '{{result}} = Trying(10)'
              - expression: 'count_words({{result}})'
                return: 'The {{result}} is 10'
              - expression: "count"
                return: !expression 'count'
            nl:
              - statement: '{{result}} = Proberen(10)'
              - expression: 'tel_woorden({{result}})'
                return: 'Het {{result}} is 10'
              - expression: "tellen"
                return: !expression 'tellen'
  - tab: "task3"
    testcases: !natural_language
        en:
          - statement: '{{result}} = Trying(10)'
          - expression: 'count_words({{result}})'
            return: 'The {{result}} is 10'
          - expression: "count"
            return: !expression 'count'
        nl:
          - statement: '{{result}} = Proberen(10)'
          - expression: 'tel_woorden({{result}})'
            return: 'Het {{result}} is 10'
          - expression: "tellen"
            return: !expression 'tellen'
""".strip()


def test_natural_translate_unit_test():
    # Everywhere where !natural_language is used, it is mandatory to do so.
    # Everywhere else it isn't.
    translated_yaml_str = """
tabs:
- tab: '{animal_tab}_{results}'
  contexts:
  - testcases:
    - statement: results_context = Trying(10)
    - expression: count_words(results_context)
      return: The results_context is 10
    - expression: count
      return: !expression 'count'
    - expression: ok(10)
      return: !oracle
        value: The results_context 10 is OK!
        oracle: custom_check
        file: test.py
        name: evaluate_test
        arguments:
        - The value
        - is OK!
        - is not OK!
      description: Ten
    files:
    - name: file.txt
      url: media/workdir/file.txt
  - testcases:
    - statement: result = Trying(11)
    - expression: result
      return: 11_{elf}
      description:
        description: Eleven_eleven
        format: code
- tab: animals
  testcases:
  - expression: tests(11)
    return: 11
  - expression:
      javascript: animals_javascript(1 + 1)
      typescript: animals_typescript(1 + 1)
      java: Submission.animals_java(1 + 1)
      python: animals_python_en(1 + 1)
    return: 2
- tab: test
  testcases:
  - expression: 'select(''a'', {''a'': 1, ''b'': 2})'
    return: 1
- tab: task
  contexts:
  - testcases:
    - statement: results = Trying(10)
    - expression: count_words(results)
      return: The results is 10
    - expression: count
      return: !expression 'count'
- tab: task2
  contexts:
  - testcases:
    - statement: results = Trying(10)
    - expression: count_words(results)
      return: The results is 10
    - expression: count
      return: !expression 'count'
- tab: task3
  testcases:
  - statement: results = Trying(10)
  - expression: count_words(results)
    return: The results is 10
  - expression: count
    return: !expression 'count'
""".strip()
    environment = create_enviroment()
    parsed_yaml = parse_yaml(test_unit_yaml_str)
    translated_dsl = translate_yaml(parsed_yaml, {}, "en", environment)
    translated_yaml = convert_to_yaml(translated_dsl)
    assert translated_yaml.strip() == translated_yaml_str


def test_natural_translate_io_test():
    # Everywhere where !natural_language is used, it is mandatory to do so.
    # Everywhere else it isn't.
    yaml_str = """
units:
  - unit: !natural_language
      en: "Arguments"
      nl: "Argumenten"
    translations:
      User:
        en: "user"
        nl: "gebruiker"
    cases:
      - script:
        - stdin: !natural_language
            en: "User_{{User}}"
            nl: "Gebruiker_{{User}}"
          arguments: !natural_language
            en: [ "input_{{User}}", "output_{{User}}" ]
            nl: [ "invoer_{{User}}", "uitvoer_{{User}}" ]
          stdout: !natural_language
            en: "Hi {{User}}"
            nl: "Hallo {{User}}"
          stderr: !natural_language
            en: "Nothing to see here {{User}}"
            nl: "Hier is niets te zien {{User}}"
          exception: !natural_language
            en: "Does not look good"
            nl: "Ziet er niet goed uit"
        - stdin: !natural_language
            en: "Friend of {{User}}"
            nl: "Vriend van {{User}}"
          arguments: !natural_language
            en: [ "input", "output" ]
            nl: [ "invoer", "uitvoer" ]
          stdout:
            data: !natural_language
              en: "Hi Friend of {{User}}"
              nl: "Hallo Vriend van {{User}}"
            config:
              ignoreWhitespace: true
          stderr:
            data: !natural_language
              en: "Nothing to see here {{User}}"
              nl: "Hier is niets te zien {{User}}"
            config:
              ignoreWhitespace: true
          exception:
            message: !natural_language
              en: "Does not look good {{User}}"
              nl: "Ziet er niet goed uit {{User}}"
            types:
              typescript: "ERROR"
  - unit: "test"
    scripts:
      - expression: !natural_language
            en: "tests(11)"
            nl: "testen(11)"
        return: 11
  - unit: "test2"
    scripts: !natural_language
      en: 
        - expression: "tests(11)"
          return: 11
      nl:
        - expression: "testen(11)"
          return: 11
""".strip()
    translated_yaml_str = """
units:
- unit: Arguments
  cases:
  - script:
    - stdin: User_user
      arguments:
      - input_user
      - output_user
      stdout: Hi user
      stderr: Nothing to see here user
      exception: Does not look good
    - stdin: Friend of user
      arguments:
      - input
      - output
      stdout:
        data: Hi Friend of user
        config:
          ignoreWhitespace: true
      stderr:
        data: Nothing to see here user
        config:
          ignoreWhitespace: true
      exception:
        message: Does not look good user
        types:
          typescript: ERROR
- unit: test
  scripts:
  - expression: tests(11)
    return: 11
- unit: test2
  scripts:
  - expression: tests(11)
    return: 11
""".strip()
    enviroment = create_enviroment()
    yaml_object = parse_yaml(yaml_str)
    translated_dsl = translate_yaml(yaml_object, {}, "en", enviroment)
    translated_yaml = convert_to_yaml(translated_dsl)
    assert translated_yaml.strip() == translated_yaml_str


def test_validation():
    yaml_object = parse_yaml(test_unit_yaml_str)
    validate_pre_dsl(yaml_object)

    enviroment = create_enviroment()
    translated_data = translate_yaml(yaml_object, {}, "en", enviroment)
    translated_yaml_string = convert_to_yaml(translated_data)
    _validate_dsl(_parse_yaml(translated_yaml_string))


def test_wrong_natural_translation_suite():
    yaml_str = """
tabs:
- tab: animals
  testcases:
  - expression: tests(11)
    return: 11
  - expression:
      javascript: animals_javascript(1 + 1)
      typescript: animals_typescript(1 + 1)
      java: Submission.animals_java(1 + 1)
      python:
        en: animals_python_en(1 + 1)
        nl: animals_python_nl(1 + 1)
    return: 2
    """.strip()
    parsed_yaml = parse_yaml(yaml_str)
    try:
        validate_pre_dsl(parsed_yaml)
    except ExceptionGroup:
        print("As expected")
    else:
        assert False, "Expected ExceptionGroup error"


def test_run_is_correct(mocker: MockerFixture):
    s = mocker.spy(tested.nat_translation, name="generate_new_yaml")  # type: ignore[reportAttributeAccessIssue]
    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in [
            """
tabs:
- tab: task3
  testcases:
  - statement: !natural_language
        nl: resultaten = Proberen(10)
        en: results = Tries(10)
  - expression: !natural_language
        nl: tel_woorden(resultaten)
        en: count_words(results)
    return: !natural_language
        nl: Het resultaat is 10
        en: The result is 10"""
        ]
    ]
    mock_files.append(mocker.mock_open(read_data="{}").return_value)
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)

    yaml_object = run_translation(Path("suite.yaml"), "en", False)

    assert s.call_count == 0
    assert isinstance(yaml_object, dict)
    tabs = yaml_object["tabs"]
    assert isinstance(tabs, list)
    assert tabs[0]["testcases"][0] == {"statement": "results = Tries(10)"}
    assert tabs[0]["testcases"][1] == {
        "expression": "count_words(results)",
        "return": "The result is 10",
    }


def test_run_is_correct_when_no_file():

    try:
        run_translation(Path("suite.yaml"), "en", False)
    except FileNotFoundError:
        print("As expected")
    else:
        assert False, "Expected FileNotFoundError error"
