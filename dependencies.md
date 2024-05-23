# Dependencies for TESTed

This file contains an overview of the various dependencies used by TESTed, and which versions are known to work. For
each language, we list the required dependencies, and their install instructions.

## I just want a Docker file

See https://github.com/dodona-edu/docker-images/blob/master/dodona-tested.dockerfile

You can also take a look at the `flake.nix` file, as it will also install most dependencies.

## Languages

All versions in this document are known to work; other versions might also work.
For example, the versions in the table below are the ones we currently use to run the test suite.

| Language            | Version |
|---------------------|---------|
| Python              | 3.11    |
| Bash                | 5.1     |
| gcc (C)             | 10      |
| ghc (Haskell)       | 8 or 9  |
| Java                | 17      |
| NodeJS (Javascript) | 18      |
| Kotlin              | 1.8     |
| C# (.NET 6)         | 10.0    |

## Core

The Python dependencies are a special case, since TESTed itself is also written in Python.
Therefore, the core Python dependencies are not optional.

Installing dependencies:

```shell
$ pip install -r requirements.txt
# Only needed if you want to run tests
$ pip install -r requirements-tests.txt
```

## Python


| Name     | Versions | Installation |
|----------|----------|--------------|
| `pylint` | 2.17     | Pip package  |

Install the package as follows:

```bash
$ pip install pylint
```

## Bash

| Name         | Versions | Installation |
|--------------|----------|--------------|
| `shellcheck` | 0.8      | OS package   |

## C

| Name       | Versions | Installation |
|------------|----------|--------------|
| `cppcheck` | 2.3-2.6  | OS package   |

## Haskell

| Name      | Versions | Installation          |
|-----------|----------|-----------------------|
| `aeson`   | latest   | Global cabal package* |
| `hlint`   | 3.2      | OS package            |

Installing global cabal packages can be done as follows:

```shell
$ cabal v1-install aeson
```

## Java

| Name         | Versions | Installation |
|--------------|----------|--------------|
| `checkstyle` | >= 8     | OS package   |

## Javascript

| Name                    | Versions | Installation |
|-------------------------|----------|--------------|
| `eslint`                | 8.36     | npm package  |
| `abstract-syntax-tree`  | 2.16     | npm package  |

Install npm packages as follows:

```shell
$ npm install eslint@8.36 abstract-syntax-tree@2.16
```

## Kotlin

| Name                   | Versions | Installation |
|------------------------|----------|--------------|
| `klint`                | 0.48     | OS package   |


## C#

C# does not have other dependencies besides .NET 6.
