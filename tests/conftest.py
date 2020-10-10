def pytest_configure(config):
    config.addinivalue_line("markers", "slow: mark tests as slow")
    config.addinivalue_line("markers", "flaky: mark tests as flaky")
