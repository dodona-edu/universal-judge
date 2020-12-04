# TESTed: universal judge for educational software testing

Organisation of the repository:

- `docker`: Docker files for creating a Docker image of the judge
- `exercise`: contains a test exercise (TODO: move this to somewhere else)
- `tested`: Python project containing the code of the actual judge that will be run by Dodona
- `tests`: Tests for TESTed
- `run` (file): Needed for the Docker image, starts the judge
- `thesis`: LaTeX files for the actual text. See the readme in that folder for compilation instructions.


## Judge

This is the judge for the TESTed framework.

The source code resides under `src`. The whole judge is implemented as a Python package called `tested`.

This means it should be run as one:

```shell script
python -m tested
```

This will execute the judge expecting a configuration on `stdin` and will print Dodona-output on `stdout`.

Other modes are also available:

- `python -m tested.testplan` will print the JSON Schema of the testplan.
- `python -m tested.manual` will run a hard-coded exercise and solution with logs enabled.
- `python -m tested.serialisation` will print the JSON Schema for only the serialization format (this is also included if you print the testplan schema).
- _`python -m tested.translate_dsl`_ will a DSL testplan to a JSON testplan


Tests should also be run from this directory:

```shell script
python -m pytest tests/test_functionality.py
```
