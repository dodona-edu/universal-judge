from tested.dsl.translate_parser import parse_dsl
from tested.serialisation import FunctionCall
from tested.testsuite import ContentPath


def test_dsl_tab_legacy_fields():
    # 'unit' is a legacy field for 'tab'
    # 'cases' is a legacy field for 'contexts'
    yaml_str = """
- unit: 'Legacy Tab'
  cases:
    - script:
        - expression: 'test()'
          return: 5
    """
    suite = parse_dsl(yaml_str)
    assert suite.tabs[0].name == "Legacy Tab"
    assert len(suite.tabs[0].contexts) == 1


def test_dsl_context_legacy_testcases():
    yaml_str = """
- tab: 'Test'
  contexts:
    - testcases:
        - expression: 'test()'
          return: 5
    """
    suite = parse_dsl(yaml_str)
    assert len(suite.tabs[0].contexts[0].testcases) == 1


def test_dsl_suite_legacy_tabs():
    yaml_str = """
tabs:
  - tab: 'Test'
    contexts:
      - testcases:
          - expression: 'test()'
    """
    suite = parse_dsl(yaml_str)
    assert len(suite.tabs) == 1
    assert suite.tabs[0].name == "Test"


def test_dsl_tab_legacy_testcases():
    yaml_str = """
- tab: 'Test'
  testcases:
    - expression: 'test()'
    """
    suite = parse_dsl(yaml_str)
    assert len(suite.tabs[0].contexts) == 1
    tc = suite.tabs[0].contexts[0].testcases[0]
    assert isinstance(tc.input, FunctionCall)
    assert tc.input.name == "test"


def test_dsl_tab_legacy_scripts():
    yaml_str = """
- unit: 'Test'
  scripts:
    - expression: 'test()'
    """
    suite = parse_dsl(yaml_str)
    assert len(suite.tabs[0].contexts) == 1
    tc = suite.tabs[0].contexts[0].testcases[0]
    assert isinstance(tc.input, FunctionCall)
    assert tc.input.name == "test"


def test_dsl_top_level_list_legacy():
    yaml_str = """
- tab: 'Tab 1'
  testcases:
    - expression: 'test1()'
- tab: 'Tab 2'
  testcases:
    - expression: 'test2()'
    """
    suite = parse_dsl(yaml_str)
    assert len(suite.tabs) == 2
    assert suite.tabs[0].name == "Tab 1"
    assert suite.tabs[1].name == "Tab 2"


def test_dsl_testcase_legacy_link_files():
    yaml_str = """
- tab: 'Test'
  testcases:
    - expression: 'test()'
      files:
        - url: path/to/data.txt
          name: data.txt
    """
    suite = parse_dsl(yaml_str)
    testcase = suite.tabs[0].contexts[0].testcases[0]
    assert len(testcase.input_files) == 1
    assert testcase.input_files[0].path == "data.txt"
    assert isinstance(testcase.input_files[0].content, ContentPath)
    assert testcase.input_files[0].content.path == "path/to/data.txt"


def test_dsl_inheritance_legacy_files():
    yaml_str = """
- tab: 'Test'
  files:
    - url: tab/data.txt
      name: data.txt
  testcases:
    - expression: 'test()'
    """
    suite = parse_dsl(yaml_str)
    testcase = suite.tabs[0].contexts[0].testcases[0]
    assert len(testcase.input_files) == 1
    assert testcase.input_files[0].path == "data.txt"
    assert isinstance(testcase.input_files[0].content, ContentPath)
    assert testcase.input_files[0].content.path == "tab/data.txt"


def test_dsl_mixed_files_input_files_priority():
    yaml_str = """
- tab: 'Test'
  files:
    - url: tab/data.txt
      name: data.txt
  testcases:
    - expression: 'test()'
      input_files:
        - path: local.txt
          content: "local content"
    """
    suite = parse_dsl(yaml_str)
    testcase = suite.tabs[0].contexts[0].testcases[0]
    # input_files should override inherited files
    assert len(testcase.input_files) == 1
    assert testcase.input_files[0].path == "local.txt"
    assert testcase.input_files[0].content == "local content\n"
