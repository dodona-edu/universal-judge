from .regenerate import all_submissions


def pytest_generate_tests(metafunc):
    if "student" in metafunc.fixturenames and "submission" in metafunc.fixturenames:
        final_data = all_submissions()
        metafunc.parametrize(["student", "submission"], final_data)


def pytest_configure(config):
    config.addinivalue_line("markers", "slow: mark tests as slow")
