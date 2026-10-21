import sys


def echo(content):
    if content == "exit":
        sys.exit(3)
    return content
