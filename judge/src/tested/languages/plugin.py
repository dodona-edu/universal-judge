"""
Support for programming languages and other related stuff is done by implementing
a plugin.

Currently, there are three types of plugins:

- LanguagePlugin: the core plugin for language support within TESTed.
- OutputProcessor: plugins that do something with the output produced by TESTed. At
  the moment, support here is limited to processing compiler output.
- SubmissionProcessor: plugins that take the submission and do something with it,
  for example a linter.


To create a language, you must implement a module with a top-level function called
"register". This function will be called with a registry, which can be used to
register various plugins.

For example, a dummy language "simple" in "tested.languages.simple.__init__.py":

```python

def register(registry: Registry):
    pass





"""
from typing import Dict

class LanguagePlugin:
    pass


class Registry:
    __slots__ = ["plugins"]

    def __init__(self):
        self.plugins: Dict[str, LanguagePlugin]

    def register(self, language: str, plugin: LanguagePlugin):
        if language in self.plugins:
            raise ValueError(f"A plugin for {language} exists already!")




