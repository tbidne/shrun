<div align="center">

# Shell Run

![cabal](https://github.com/tbidne/shell-run/workflows/cabal/badge.svg?branch=main)
![hlint](https://github.com/tbidne/shell-run/workflows/hlint/badge.svg?branch=main)
![ormolu](https://github.com/tbidne/shell-run/workflows/ormolu/badge.svg?branch=main)

</div>

---

### Table of Contents
- [Motivation](#motivation)
- [Introduction](#introduction)
- [Building](#building)
- [Tests](#tests)

# Motivation

This app was borne out of frustration when running shell commands. Say, for instance, you run several commands on a regular basis, e.g., updates after pulling the latest code. You can run these manually like:

```sh
cmd1
cmd2
cmd3
...
```

But that can be a lot of repetitive typing, especially when the commands are longer. Thus you write an alias:

```sh
alias run_commands="cmd1 && cmd2 && cmd3 ..."
```

All well and good, but this approach has several deficiencies:

1. You do not receive any information about how long your commands have been running. If any of the commands are long-lived, how do you know when it's been "too long" and you should cancel them? You can look at a clock or use a stopwatch, but that requires you to remember every time you run the command, which is certainly unsatisfying.

1. These commands are all run synchronously even though there may be no relation between them. E.g., if you have two commands that each take 5 minutes, the combination will take 10 minutes. This is usually unnecessary.

1. Related to above, if any command fails then subsequent ones will not be run. This can be frustrating, as you may kick off a run and leave, only to return and find out that later, longer-running commands never ran because of some trivial error in the beginning.

1. It does not scale. Imagine you have variations of `cmd3` you want to run under different circumstances. You could create multiple aliases:

```sh
alias run_commands_cmd3a="cmd1 && cmd2 && cmd3a"
alias run_commands_cmd3b="cmd1 && cmd2 && cmd3b"
```

But this is messy and obviously does not scale. You would need an exponential number of aliases for each variation.

This application purports to overcome these limitations.

# Introduction

The application has the following usage:
```sh
Usage: shell-run [-l|--legend ARG] [-t|--timeout ARG] Commands...
```

In general, `shell-run` is a wrapper around running shell commands. For instance:

```sh
shell-run "some long command" "another command"
```

Will run `some long command` and `another command` concurrently.

A running timer is provided, and stdout/stderr will be updated when a command finishes/crashes, respectively.

## Options

### Timeout

A timeout can be provided via `-t <integer>` or `--timeout=<integer>`.


If a timeout is provided, it must be a non-negative integer. If the timeout is reached, then all remaining commands will be cancelled.

### Legend

A legend file can be specified by `-l <path/to/legend>` or `--legend=<path/to/legend>`.


Lines are formatted `<cmd_key>=<command value>` (no angle brackets).

Each line can be separated by as many new lines as desired, and comment lines start with a #. Command values themselves can include multiple commands delimited by commas, and they may reference other commands. For instance, given a legend file:

```text
cmd1=echo "command one"

# recursive references
cmd2=cmd1
cmd3=cmd2

cmd4=command four

# runs 3 and 4
all=cmd3,cmd4,echo hi
```

Then the command

```sh
shell-run --legend=path/to/legend all "echo cat"
```

Will run `echo "command one"`, `command four`, `echo hi` and `echo cat` concurrently.

# Building

## Prerequisites

- `cabal-install 3.4`
- `ghc 8.10.4+`

The app can be built via `cabal`. If you are using `nix`, a `shell.nix` file exists that will provide the needed dependencies, including the right `ghc`. Otherwise, building is the same as `cabal`.

## Cabal

You will need `ghc 8.10.4+` and `cabal-install 3.4`. From there the app can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

The project is set to build with `-Werror` in `cabal.project`, so if for some reason that's a problem, you can disable this with `cabal build --ghc-options="-Wwarn"`.

# Tests

There are two test suites, `unit` and `functional`. These can be run via:

```sh
# --test-show-details=direct gives nicer output
cabal test unit --test-show-details=direct
cabal test functional --test-show-details=direct

# everything
cabal test --test-show-details=direct
```