# TESTed: universal judge for educational software testing

TESTed is a software test framework to evaluate submissions for programming exercises across multiple programming languages, using a single test suite per exercise.

TESTed is developed by [Team Dodona](https://dodona.ugent.be/en/about/) at Ghent University.
If you use this software in research, please cite:

- Strijbol, N., Van Petegem, C., Maertens, R., Sels, B., Scholliers, C., Dawyndt, P., & Mesuere, B. (2023). TESTed—An educational testing framework with language-agnostic test suites for programming exercises. SoftwareX, 22, 101404. [doi:10.1016/j.softx.2023.101404](https://doi.org/10.1016/j.softx.2023.101404)

> [!IMPORTANT]
> The documentation below is intended for running TESTed as a standalone tool.
> If you are looking to create exercises for Dodona, we have [more suitable documentation available](https://docs.dodona.be/nl/guides/exercises/).


## Installing TESTed

TESTed is implemented in Python, but has various dependencies for its language-specific modules.

To be able to work with all these dependencies on different platforms we make use of [devcontainers](https://containers.dev/).
This means that you can use the provided `.devcontainer/devcontainer.json` to open a container with all dependencies installed.

Modern IDEs like [Visual Studio Code](https://code.visualstudio.com/) and [PyCharm](https://www.jetbrains.com/pycharm/) support devcontainers out of the box.

If you prefer installing all dependencies on your local machine, you can find the installed dependencies in the [dockerfile](./.devcontainer/dodona-tested.dockerfile).
The extra development dependencies are listed in the [dev-dependencies.sh](./devcontainer/dev-dependencies.sh) file.

## Running TESTed

TESTed evaluates a submission for a programming exercise based on a test suite that specifies some test cases for the exercise.
In what follows, we guide you through the configuration of a simple programming exercise and running TESTed to evaluate a submission using the test suite of the exercise.
The directory `./exercise/` in the root directory of TESTed contains some more examples of programming exercises with test suites for TESTed.

### 1. Create an exercise

Let's configure a simple programming exercise that asks to implement a function `echo`.
The function takes a single argument and returns its argument.

Start creating a directory for the configuration of the exercise.
To keep things simple, we add the exercise to the `exercise` subdirectory in the root directory of TESTed.

```bash
mkdir exercise/simple-example
```

Note that you would normally not store your exercises in the TESTed repository.
We recommend creating a new repository for your exercises.

### 2. Create a test suite

The next step is to design a test suite that will be used to evaluate submission for the exercise.
Again, to keep things simple, we will only include a single test case in the test suite.

```yaml
- tab: Echo
  testcases:
    - expression: "echo('input-1')"
      return: "input-1"
```

This test suite describes the following tests: we have one tab, which is named `Echo`.
Inside this tab, there is one test case, in which we call the function `echo` with the string argument `"input-1"`.
The expected output is a return value (again a string) of `"input-1"`.
All other tests use the defaults: for example, no output is allowed on stderr, while stdout is ignored. 

Put the file containing the test suite in the following location:

```bash
# Create the file
$ touch exercise/simple-example/suite.yaml
# Now you should put the content from above in the file.
```

### 3. Create some submissions

Now create two Python submissions for the programming exercise.
The first one contains a correct solution, and the second one returns the wrong result.

```bash
$ cat exercise/simple-example/correct.py
def echo(argument):
  return argument
$ cat exercise/simple-example/wrong.py
def echo(argument):
  # Oops, this is wrong.
  return argument * 2
```

### 4. Evaluate the submissions

To evaluate a submission with TESTed, you need to provide a test suite and configuration information.
This information can be piped to TESTed via stdin, but to make things easier, we will add the information to a configuration file in the directory of the exercise.
In practice, this configuration file would be created by the learning environment in which TESTed is integrated.

```bash
$ cat exercise/simple-example/config.json
{
  "programming_language": "python",
  "natural_language": "en",
  "resources": "exercise/simple-example/",
  "source": "exercise/simple-example/correct.py",
  "judge": ".",
  "workdir": "workdir/",
  "test_suite": "suite.yaml",
  "memory_limit": 536870912,
  "time_limit": 60
}
```

These attributes are used by TESTed:

- `programming_language`: programming language of the submission
- `resources`: path of a directory with resources TESTed can use
- `source`: path of the submission that must be evaluated
- `judge`: path of the root directory of TESTEd
- `workdir`: path of a temporary directory (see below)
- `test_suite`: path of the test suite, relative to the resources directory (as defined above)

Before evaluating a submission, TESTed generates test code in the workdir.
Create that directory:

```bash
$ mkdir workdir/
```

The content in this directory stays in place after TESTed finishes its evaluation, so you can inspect the generated test code.
Before running TESTed again, you'll need to clear this directory.

With this command, TESTed will evaluate the submission and generate feedback on stdout.

```bash
$ python -m tested -c exercise/simple-example/config.json
{"command": "start-judgement"}
{"title": "Echo", "command": "start-tab"}
{"command": "start-context"}
{"description": {"description": "echo('input-1')", "format": "python"}, "command": "start-testcase"}
{"expected": "input-1", "channel": "return (String)", "command": "start-test"}
{"generated": "input-1", "status": {"enum": "correct"}, "command": "close-test"}
{"command": "close-testcase"}
{"command": "close-context"}
{"command": "close-tab"}
{"command": "close-judgement"}
```
By default, TESTed generates its feedback on stdout. The feedback is formatted in the [JSON Lines](https://jsonlines.org/) text format, meaning that each line contains a JSON object. Here's how you get an overview of all options supported by TESTed:

```bash
$ python -m tested --help
usage: __main__.py [-h] [-c CONFIG] [-o OUTPUT] [-v]

The programming-language-agnostic educational test framework.

optional arguments:
  -h, --help            show this help message and exit
  -c CONFIG, --config CONFIG
                        Where to read the config from
  -o OUTPUT, --output OUTPUT
                        Where the judge output should be written to.
  -v, --verbose         Include verbose logs. It is recommended to also use -o in this case.
```

Adjust the configuration file if you want to evaluate the wrong submission.

For reference, the file `tested/dsl/schema.json` contains the JSON Schema of the test suite format.

## Running TESTed locally

The `python -m tested` command is intended for production use.
However, it is not always convenient to create a `config.json` file for each exercise to run.

TESTed supports two ways of running TESTed without a config file.
The first way is:

```bash
# Run a hard-coded exercise with logs enabled, useful for debugging
$ python -m tested.manual
```

This command is useful when debugging TESTed itself or a particularly challenging exercise.
It will execute a hardcoded config, which is set in `tested/manual.py`.

The second way is:

```bash
# Run an exercise with CLI paramaters
$ python -m tested.cli --help
usage: cli.py [-h] -e EXERCISE [-s SUBMISSION] [-t TESTSUITE] [-f] [-v] [-d] [-p PROGRAMMING_LANGUAGE]

Simple CLI for TESTed

options:
  -h, --help            show this help message and exit
  -e EXERCISE, --exercise EXERCISE
                        Path to a directory containing an exercise
  -s SUBMISSION, --submission SUBMISSION
                        Path to a submission to evaluate
  -t TESTSUITE, --testsuite TESTSUITE
                        Path to a test suite
  -f, --full            If the output should be shown in full (default: false)
  -v, --verbose         If the judge should be verbose in its output (default: false)
  -d, --debug           If the judge should be outputing the debug messages (default: false)
  -p PROGRAMMING_LANGUAGE, --programming_language PROGRAMMING_LANGUAGE
                        The programming language to use

additional information: The CLI only looks at a config.json file in the exercise directory. It does not look in folders above the exercise directory.
```

This is the "CLI mode": here you can pass various options as command line parameters.
For example, for exercises following a standardized directory structure, the path to the exercise folder is often enough.

## TESTed repository

The repository of TESTed is organized as follows:

- `tested`: Python code of the actual judge (run by Dodona)
- `tests`: unit tests for TESTed


## Useful commands

You can run the basic unit tests with:

```bash
pytest tests/test_functionality.py
```

You can run the full test suite with:

```bash
pytest -n auto tests/
```

We use `black` and `isort` for code formatting. `pyright` is used for type checking.
You can run them with:

```bash
black ./tested ./tests
isort ./tested ./tests
pyright ./tested ./tests
```


