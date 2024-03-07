<div align="center">

# Shrun

## Run Shell Commands Concurrently

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/shrun?include_prereleases&sort=semver&labelColor=2f353e)](https://github.com/tbidne/shrun/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/shrun/ci.yaml?branch=main)](https://github.com/tbidne/shrun/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/shrun?color=blue&labelColor=2f353e)](https://opensource.org/licenses/MIT)

![linux](https://img.shields.io/static/v1?label=&message=linux&logo=linux&logoColor=white&labelColor=2f353e&color=blue)
![apple](https://img.shields.io/static/v1?label=&message=osx&logo=apple&labelColor=2f353e&color=blue)

![demo](./examples/demo.gif)

</div>

---

### Table of Contents
- [Motivation](#motivation)
- [Introduction](#introduction)
- [Configuration](#configuration)
- [Building](#building)
  - [Cabal](#cabal)
  - [Nix](#nix)
- [FAQ](#faq)

# Motivation

`shrun` was borne of frustration. Suppose we need to run several shell commands on a regular basis e.g. updates after pulling the latest code. These can be run manually in separate terminals:

```sh
cmd1
cmd2
cmd3
...
```

But this can be a lot of repetitive typing, especially when the commands are longer. Thus an alias is born:

```sh
alias run_commands="cmd1 && cmd2 && cmd3 ..."
```

All well and good, but this approach has several deficiencies:

1. There is no information about how long the commands have been running. If any of the commands are long-lived, how would we know when it has been "too long" and the commands should be cancelled? We could use a wall clock or a stopwatch, but that is imprecise and requires remembering every time the commands are run, which is certainly unsatisfying.

1. These commands are all run synchronously even though there may be no relation between them. For example, three commands that each take 5 minutes will combine to take 15 minutes. This is usually unnecessary.

1. Related to above, if any command fails then subsequent ones will not be run. This can be frustrating, especially when a quicker command in the beginning prevents a longer one at the end from even starting.

1. If the alias is tweaked to run all regardless (`cmd1; cmd2; cmd3 ...`), then it can be difficult to determine which, if any, failed. Additionally, understanding logs is much harder.

1. It does not scale. Imagine we have variations of `cmd3` we want to run under different circumstances. We could create multiple aliases:


        alias run_commands_cmd3a="cmd1 && cmd2 && cmd3a"
        alias run_commands_cmd3b="cmd1 && cmd2 && cmd3b"

    But this is messy and grows exponentially in the number of aliases for each variation.

`shrun` purports to overcome these limitations.

# Introduction

In a nut-shell (😉), `shrun` is a wrapper around running shell commands. For instance:

```sh
shrun "some long command" "another command"
```

Will run `some long command` and `another command` concurrently.

A running timer is provided, and stdout will be updated when a command finishes or crashes.

Note: `shrun` colors its logs, and the examples shown here _should_ use these colors. Unfortunately github does not render them, so you will have to view this markdown file somewhere else to see them.

# Configuration

See [configuration.md](configuration.md) for the available options.

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`ghcup`](https://www.haskell.org/ghcup/)

Using `ghcup`, install `cabal 2.4+` and one of:

- `ghc 9.4`
- `ghc 9.6`
- `ghc 9.8`

### Build Shrun

Once you have `cabal` and `ghc`, `shrun` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Nix

### Prerequisites

* [nix](https://nixos.org/download.html)

### Manually

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `shrun` can be built with `nix build`, which will compile and run the tests.

### Nix expression

Because `shrun` is a flake, it can be built as part of a nix expression. For instance, if you want to add `shrun` to `NixOS`, your `flake.nix` should have:

```nix
# flake.nix
{
  inputs.shrun.url = "github:tbidne/shrun/main";
}
```

Then include this in the `systemPackages`:

```nix
# wherever your global packages are defined
{
  environment.systemPackages = [
    shrun.packages."${system}".default
  ];
}
```

# FAQ

## What if a command needs sudo?

Commands _can_ receive `stdin`, so running e.g. `shrun "sudo some command"` will launch the sudo prompt. From there we can type the password and hit `enter` as usual.

However this is a bit clunky as the timer text will overwrite the `[sudo] password for ...` prompt, and multiple commands add more complication.

It is therefore easiest to run sudo first to elevate privileges, then execute `shrun` as normal e.g.

```
# Run sudo with some dummy command
$ sudo ls
...

# shrun can now execute sudo without requiring stdin
$ shrun ...
```

## What if my command relies on interactive shell e.g. loading ~/.bashrc?

Shrun executes shell commands non-interactively, which means we do not have access to anything defined in, say, `~/.bashrc`. This can be annoying if we want to run any of these functions/aliases.

```sh
# ~/.bashrc
foo () {
  ...
}
```

```
$ shrun foo
[Error][foo] 0 seconds: /bin/sh: line 1: foo: command not found
[Finished] 0 seconds
```

Fortunately, the [Init](configuration.md#init) option exists exactly for this purpose:

```
$ shrun --init ". ~/.bashrc" foo
```

This is equivalent to running:

```
$ shrun ". ~/.bashrc && foo"
```