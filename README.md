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
- [Installation](#installation)
- [Building](#building)
  - [Cabal](#cabal)
  - [Stack](#stack)
  - [Nix](#nix)
- [Configuration](#configuration)
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

# Installation

The [releases](https://github.com/tbidne/shrun/releases) page has binaries built for several platforms. If there are no binaries for your platform, it is possible to [build shrun](#building) yourself.

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`cabal 3.8+`](https://www.haskell.org/cabal/download.html)
* One of:
  * [`ghc 9.4`](https://www.haskell.org/ghc/download.html)
  * [`ghc 9.6`](https://www.haskell.org/ghc/download.html)
  * [`ghc 9.8`](https://www.haskell.org/ghc/download.html)

The easiest way to install these is generally [`ghcup`](https://www.haskell.org/ghcup/).

The current "blessed" version is `ghc-9.6.4`.

### Build Shrun

Once you have `cabal` and `ghc`, `shrun` can be built locally with `cabal build` or installed globally (e.g. `~/.local/bin/shrun`) with `cabal install`.

For further reproducibility, optional freeze files can be used e.g.

```sh
cabal build --project-file cabal.ghc964.project
```

> [!NOTE]
>
> Freeze files are provided for only select compilers.

## Stack

### Prerequisites

* [`stack`](https://docs.haskellstack.org/en/stable/)

Like `cabal` and `ghc`, `stack` can be installed with [`ghcup`](https://www.haskell.org/ghcup/).

### Build Shrun

Once you have `stack`, `shrun` can be built with `stack build` or installed globally (i.e. `~/.local/bin/shrun`) with `stack install`.

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

# Configuration

See [configuration.md](configuration.md) for the available options.

# FAQ

See [faq.md](faq.md).
