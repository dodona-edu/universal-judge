"""
The main judge package. Responsible for turning a configuration bundle (containing
the configs, test suite and solution) into output for Dodona.

The main module has two functions, which are the main interfaces to interact with
the judge. All other modules might be useful, but are more for the internal code
organization.
"""
from tested.judge.core import judge
