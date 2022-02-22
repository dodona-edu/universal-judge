# Dependencies for TESTed

This file contains an overview of the various dependencies used by TESTed, and which versions are known to work. For
each language, we list the required dependencies, and their install instructions.

## I just want a Docker file

See https://github.com/dodona-edu/docker-images/blob/master/dodona-tested.dockerfile

You can also take a look at the `.github/workflows/ci.yml` file, as it will also install most dependencies.

## Languages

All versions in this document are known to work; other versions might also work. For example, the version in the table
below are the ones we currently use to run the test suite.

| Language            | Version |
|---------------------|---------|
| Python              | 3.9     |
| Bash                | 5.1     |
| gcc (C)             | 10.3    |
| ghc (Haskell)       | 9.2     |
| Java                | 17      |
| NodeJS (Javascript) | 16      |
| Kotlin              | 1.6     |

## Core

The Python dependencies are a special case, since TESTed itself is also written in Python. Therefor, the core Python
dependencies are not optional.

Installing dependencies:

```shell
$ pip install -r requirements.txt
# Only needed if you want to run tests
$ pip install -r requirements-tests.txt
```

## Python

The dependencies needed to evaluate Python submissions are listed in `tested/languages/python/requirements.txt`.

```shell
$ pip install -r tested/languages/python/requirements.txt
```

## Bash

| Name         | Versions | Installation |
|--------------|----------|--------------|
| `shellcheck` | 0.8      | OS package   |

## C

| Name       | Versions | Installation |
|------------|----------|--------------|
| `cppcheck` | 2.6      | OS package   |

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
| `checkstyle` | 9.2      | OS package   |

## Javascript

| Name                    | Versions | Installation |
|-------------------------|----------|--------------|
| `eslint`                | 8.7      | npm package  |
| `abstract-syntax-tree`  | 2.16     | npm package  |

Install npm packages as follows:

```shell
$ npm install eslint@8.7 abstract-syntax-tree@2.16
```

## Kotlin

| Name                   | Versions | Installation |
|------------------------|----------|--------------|
| `klint`                | 0.43     | OS package   |

