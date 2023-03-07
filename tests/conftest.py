def pytest_configure(config):
    config.addinivalue_line("markers", "slow: mark tests as slow")
    config.addinivalue_line("markers", "linter: run linter tests")
    config.addinivalue_line("markers", "haskell: mark as an Haskell exercise")
