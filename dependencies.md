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
| Python              | 3.12    |
| Bash                | 5.2     |
| gcc (C)             | 13      |
| ghc (Haskell)       | 9.6     |
| Java                | 21      |
| NodeJS (Javascript) | 22      |
| Kotlin              | 2.0     |
| C# (.NET 8)         | 12.0    |

## Core

The Python dependencies are a special case, since TESTed itself is also written in Python.
Therefore, the core Python dependencies are not optional.

Installing dependencies:

```shell
$ poetry install
```

## Python


| Name     | Versions | Installation |
|----------|----------|--------------|
| `pylint` | 3.2      | Pip package  |

Install the package as follows:

```bash
$ pip install pylint
```

## Bash

| Name         | Versions | Installation |
|--------------|----------|--------------|
| `shellcheck` | 0.10     | OS package   |

## C

| Name       | Versions | Installation |
|------------|----------|--------------|
| `cppcheck` | 2.14     | OS package   |

## Haskell

| Name      | Versions | Installation          |
|-----------|----------|-----------------------|
| `aeson`   | latest   | Global cabal package* |
| `hlint`   | 3.6      | OS package            |

Installing global cabal packages can be done as follows:

```shell
$ cabal v1-install aeson
```

## Java

| Name         | Versions | Installation |
|--------------|----------|--------------|
| `checkstyle` | 10.16    | OS package   |

## Javascript

| Name                    | Versions | Installation |
|-------------------------|----------|--------------|
| `eslint`                | 9.10     | npm package  |
| `abstract-syntax-tree`  | 2.22     | npm package  |

Install npm packages as follows:

```shell
$ npm install eslint@9.10 abstract-syntax-tree@2.22
```

## Kotlin

| Name                   | Versions | Installation |
|------------------------|----------|--------------|
| `klint`                | 1.2      | OS package   |


## C#

C# does not have other dependencies besides .NET 8.
