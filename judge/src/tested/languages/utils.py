from tested.languages.config import Config


def memory_limit_jvm(config: Config) -> int:
    """
    Get the memory limit in bytes. Java requires this to be a multiple of 1024.
    See https://docs.oracle.com/en/java/javase/14/docs/specs/man/java.html
    """
    limit = int(config.memory_limit)
    limit = (limit // 1024) * 1024
    return limit
