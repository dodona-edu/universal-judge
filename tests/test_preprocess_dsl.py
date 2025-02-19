from tested.dsl.translate_parser import _parse_yaml
from tested.nat_translation import (
    convert_to_yaml,
    create_enviroment,
    parse_value,
    translate_dsl,
    validate_pre_dsl,
)


def test_natural_translate_unit_test():
    # Everywhere where !natural_language is used, it is mandatory to do so.
    # Everywhere else it isn't.
    yaml_str = """
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
  - tab: "{{ animal|braces }}_{{ '{' + result + '}' }}_{{ negentien|default('{{ negentien }}') }}"
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
            en: !expression "count"
            nl: !expression "tellen"
          return: !natural_language
            en: 'count'
            nl: 'tellen'
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
""".strip()
    translated_yaml_str = """
tabs:
- tab: '{animal_tab}_{results}_{{ negentien }}'
  contexts:
  - testcases:
    - statement: results_context = Trying(10)
    - expression: count_words(results_context)
      return: The results_context is 10
    - expression: !expression 'count'
      return: count
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
""".strip()
    parsed_yaml = _parse_yaml(yaml_str)
    translated_dsl = translate_dsl(parsed_yaml, "en")
    translated_yaml = convert_to_yaml(translated_dsl)
    print(translated_yaml)
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
""".strip()
    parsed_yaml = _parse_yaml(yaml_str)
    translated_dsl = translate_dsl(parsed_yaml, "en")
    translated_yaml = convert_to_yaml(translated_dsl)
    print(translated_yaml)
    assert translated_yaml.strip() == translated_yaml_str


def test_translate_parse():
    env = create_enviroment()
    flattened_stack = {"animal": "dier", "human": "mens", "number": "getal"}
    value = {
        "key1": ["value1_{{animal}}", "value1_{{human}}"],
        "key2": "value2_{{number}}",
        "key3": 10,
    }
    expected_value = {
        "key1": ["value1_dier", "value1_mens"],
        "key2": "value2_getal",
        "key3": 10,
    }
    parsed_result = parse_value(value, flattened_stack, env)
    assert parsed_result == expected_value


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
    parsed_yaml = _parse_yaml(yaml_str)
    try:
        validate_pre_dsl(parsed_yaml)
    except ExceptionGroup:
        print("As expected")
    else:
        assert False, "Expected ExceptionGroup, but no exception was raised"
