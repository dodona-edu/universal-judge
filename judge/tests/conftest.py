from .regenerate import all_submissions

LANGUAGES = ["python", "java", "haskell"]


def pytest_generate_tests(metafunc):
    if "student" in metafunc.fixturenames and "submission" in metafunc.fixturenames:
        final_data = all_submissions()
        metafunc.parametrize(["student", "submission"], final_data)
    # Languages
    if "language" in metafunc.fixturenames:
        metafunc.parametrize("language", LANGUAGES)
