<div align="center">

# Shrun

## Run Shell Commands Concurrently

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/shrun?include_prereleases&sort=semver)](https://github.com/tbidne/shrun/releases/)
![haskell](https://img.shields.io/static/v1?label=&message=9.4&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![MIT](https://img.shields.io/github/license/tbidne/shrun?color=blue)](https://opensource.org/licenses/MIT)

[![nix](http://img.shields.io/github/actions/workflow/status/tbidne/shrun/nix.yaml?branch=main&label=nix&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/shrun/actions/workflows/nix.yaml)
[![cabal](http://img.shields.io/github/actions/workflow/status/tbidne/shrun/cabal.yaml?branch=main&label=cabal&labelColor=2f353c)](https://github.com/tbidne/shrun/actions/workflows/cabal.yaml)
[![style](http://img.shields.io/github/actions/workflow/status/tbidne/shrun/style.yaml?branch=main&label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/shrun/actions/workflows/style.yaml)

![demo](./examples/demo.gif)

</div>

---

### Table of Contents
- [Motivation](#motivation)
- [Introduction](#introduction)
- [Configuration](#configuration)
  - [Core Functionality](#core-functionality)
    - [Config](#config)
    - [No Config](#no-config)
    - [Timeout](#timeout)
  - [Logging](#logging)
    - [Command Log](#command-log)
    - [File Log](#file-log)
    - [File Log Mode](#file-log-mode)
    - [File Log Size Mode](#file-log-size-mode)
  - [Log Formatting](#log-formatting)
    - [Key Hide](#key-hide)
    - [Strip Control](#strip-control)
    - [File Log Strip Control](#file-log-strip-control)
    - [Command Name Truncation](#command-name-truncation)
    - [Command Line Truncation](#command-line-truncation)
  - [Miscellaneous](#miscellaneous)
    - [Default Config](#default-config)
- [Building](#building)
  - [Cabal](#cabal)
  - [Nix](#nix)

# Motivation

`shrun` was borne of frustration. Suppose one needs to run several shell commands on a regular basis e.g. a developer running updates after pulling the latest code. These can be run manually in separate terminals:

```sh
cmd1
cmd2
cmd3
...
```

But this can be a lot of repetitive typing, especially when the commands are longer. Thus an alias is written:

```sh
alias run_commands="cmd1 && cmd2 && cmd3 ..."
```

All well and good, but this approach has several deficiencies:

1. There is no information about how long the commands have been running. If any of the commands are long-lived, how would we know when it has been "too long" and the commands should be cancelled? One could use a wall clock or a stopwatch, but that is imprecise and requires remembering every time the commands are run, which is certainly unsatisfying.

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

`shrun` can be configured by either CLI args or a `toml` config file. Most arguments exist in both formats -- where they have the same name -- though some exist only as CLI args. The following describes the CLI args. See [default.toml](./examples/default.toml) for a description of the `toml` file.

## Core Functionality

### Config

**Arg:** `-c, --config PATH`

**Description**: Path to TOML config file. If this argument is not given we automatically look in the Xdg config directory e.g. `~/.config/shrun/config.toml`.

Examples can be found in [examples](./examples).


#### Legend

In addition to providing an alternative to CLI args, the config file has a `legend` section. This allows one to define aliases for commands. Each alias has a key and a value. The value can either be a single unit or a list of units, where a unit is either a command literal (e.g. bash expression) or a recursive reference to another alias.

**Example:** For instance, given the section

```toml
legend = [
  { key = 'cmd1', val = 'echo "command one"' },
  { key = 'cmd2', val = 'cmd1' },
  { key = 'cmd3', val = 'cmd2' },
  { key = 'cmd4', val = 'command four' },
  { key = 'all', val = ['cmd3', 'cmd4', 'echo hi'] },
]
```

Then the command

```sh
shrun --config=path/to/config all "echo cat"
```

Will run `echo "command one"`, `command four`, `echo hi` and `echo cat` concurrently. A picture is worth a thousand words:

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --config=examples/config.toml all "echo cat"</span>
<span style="color: #69ff94">[Success][echo cat] 0 seconds</span>
<span style="color: #69ff94">[Success][echo hi] 0 seconds</span>
<span style="color: #69ff94">[Success][cmd1] 0 seconds</span>
<span style="color: #ff6e6e">[Error][cmd4] 0 seconds: /bin/sh: line 1: four: command not found</span>
<span style="color: #d6acff">[Finished] 0 seconds</span></code>
</pre>

Note: duplicate keys will cause a parse error to be thrown when loading. Cyclic keys are also disallowed, though these will only throw if you actually try to execute one (i.e. merely having cyclic definitions in the legend will not throw an error).

### No Config

**Arg:** `--no-config`

**Description**: Overrides toml file config regardless of how it was obtained i.e. explicit --config or implicit reading of the Xdg config file. This is useful when a config file exists at the expected Xdg location, but we wish to ignore it.

### Timeout

**Arg:** `-t, --timeout <NATURAL | STRING>`

**Description:** The provided timeout must be either a raw integer (interpreted as seconds), or a "time string" e.g. `1d2m3h4s`, `3h20s`. All integers must be non-negative. If the timeout is reached, then all remaining commands will be cancelled.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --timeout 8 "sleep 5" "sleep 10" "sleep 15"</span>
<span style="color: #69ff94">[Success][sleep 5] 5 seconds</span>
<span style="color: #d3d38e">[Warn] Timed out, cancelling remaining commands: sleep 10, sleep 15</span>
<span style="color: #d6acff">[Finished] 9 seconds</span></code>
</pre>

## Logging

### Command Log

**Arg:** `-l, --cmd-log`

**Description:** The default behavior is to swallow logs for the commands themselves. This flag gives each command a console region in which its logs will be printed. Only the latest log per region is shown at a given time.

Note: When commands have complicated output, the logs can interfere with each other (indeed even overwrite themselves). We attempt to mitigate such situations: see [Strip Control](#strip-control).

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log "for i in {1..10}; do echo hi; sleep 1; done"</span>
<span style="color:">[Command][for i in {1..10}; do echo hi; sleep 1; done] hi</span>
<span style="color: #a3fefe">[Timer] 7 seconds</span></code>
</pre>

vs.

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun "for i in {1..10}; do echo hi; sleep 1; done"</span>
<span style="color: #a3fefe">[Timer] 7 seconds</span></code>
</pre>

Note: Both the commands' `stdout` and `stderr` are treated the same, logged with the same formatting. This is because many shell programs perform redirection like `echo ... >&2` (i.e. redirect `stdout` to `stderr`). Not only does this mean we need to take both if we do not want to skip any output, but it also means it does not make sense to try to differentiate the two anymore, as that information has been lost.

Practically speaking, this does not have much effect, just that if a command dies while `--cmd-log` is enabled, then the final `[Error] ...` output may not have the most relevant information. See [File Log](#file-log) for details on investigating command failure.

### File Log

**Arg:** `-f, --file-log <default | PATH>`

**Description**: If a path is supplied, all logs will additionally be written to the supplied file. Furthermore, command logs will be written to the file irrespective of `--cmd-log`. Console logging is unaffected. This can be useful for investigating command failures. If the string `default` is given, then we write to the Xdg config directory e.g. `~/.config/shrun/log`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --file-log=out.log "sleep 2" "bad" "for i in {1..3}; do echo hi; sleep 1; done"</span>
<span style="color: #ff6e6e">[Error][bad] 0 seconds: /bin/sh: line 1: bad: command not found</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #69ff94">[Success][for i in {1..3}; do echo hi; sleep 1; done] 3 seconds</span>
<span style="color: #d6acff">[Finished] 3 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> cat out.log</span>
<span style="color:">[2022-12-12 23:17:55][Command][for i in {1..3}; do echo hi; sleep 1; done] hi</span>
<span style="color:">[2022-12-12 23:17:55][Command][bad] /bin/sh: line 1: bad: command not found</span>
<span style="color:">[2022-12-12 23:17:55][Error][bad] 0 seconds: /bin/sh: line 1: bad: command not found</span>
<span style="color:">[2022-12-12 23:17:56][Command][for i in {1..3}; do echo hi; sleep 1; done] hi</span>
<span style="color:">[2022-12-12 23:17:57][Success][sleep 2] 2 seconds</span>
<span style="color:">[2022-12-12 23:17:57][Command][for i in {1..3}; do echo hi; sleep 1; done] hi</span>
<span style="color:">[2022-12-12 23:17:58][Success][for i in {1..3}; do echo hi; sleep 1; done] 3 seconds</span>
<span style="color:">[2022-12-12 23:17:58][Finished] 3 seconds</span></code>
</pre>

### File Log Mode

**Arg:** `--file-log-mode <append | write>`

**Description:** Mode in which to open the log file. Defaults to write.

### File Log Size Mode

**Arg:** `--file-log-size-mode <warn SIZE | delete SIZE>`

**Description:** Sets a threshold for the file log size, upon which we either print a warning or delete the file, if it is exceeded. The `SIZE` should include the value and units e.g. `warn 10 mb`, `warn 5 gigabytes`, `delete 20.5B`.

## Log Formatting

### Key Hide

**Arg:** `-k, --key-hide`

**Description:** By default, we display the key name from the legend file over the actual command that was run, if the former exists. This flag instead shows the literal command. Commands without keys are unaffected.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --key-hide --config=examples/config.toml skynet</span>
<span style="color:">[Command][echo "preparing nuclear missil-- i mean gift baskets"; sleep 13] preparing nuclear missil-- i mean gift baskets</span>
<span style="color: #a3fefe">[Timer] 7 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --key-hide --config=examples/config.toml skynet</span>
<span style="color: #69ff94">[Success][echo "preparing nuclear missil-- i mean gift baskets"; sleep 13] 13 seconds</span>
<span style="color: #d6acff">[Finished] 13 seconds</span></code>
</pre>

rather than the usual

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log --config=examples/config.toml skynet</span>
<span style="color:">[Command][skynet] preparing nuclear missil-- i mean gift baskets</span>
<span style="color: #a3fefe">[Timer] 7 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log --config=examples/config.toml skynet</span>
<span style="color: #69ff94">[Success][skynet] 13 seconds</span>
<span style="color: #d6acff">[Finished] 13 seconds</span></code>
</pre>

Naturally, this does not affect commands that do not have a key (i.e. those not in a legend file). Also, if the commands are defined recursively, then the key name will be the _final_ key.

### Strip Control

**Arg:** `-s,--cmd-log-strip-control <all | smart | none>`

**Description:** Control characters can wreak layout havoc with the `--cmd-log` option, thus we include this option. `all` strips all such chars. `none` does nothing i.e. all chars are left untouched. The default `smart` attempts to strip only the control chars that affect layout (e.g. cursor movements) and leaves others unaffected (e.g. colors). This has the potential to be the 'prettiest' as:

* Simple formatting is left intact.
* The layout should not be damaged.

Though it is possible to miss some chars. This option is experimental and subject to change.

**Example:**

Note: In the following examples, `\033[35m` and `\033[3D` are ansi escape codes. The former sets the text color to magenta, and the latter resets the cursor to the left by 3 places i.e. partially overwrites the previous characters. We also include the options `-lx10` (show command logs and truncate command name to 10 chars) to make the output easier to read.

`all` strips _all_ control characters: `\033` in this case. The means all special formatting / control will be omitted.
<pre>
<code><span style="color: #ff79c6">$</span><span> shrun -lx10 --cmd-log-strip-control all "echo -e ' foo \033[35m hello \033[3D bye '; sleep 5"</span>
<span style="color:">[Command][echo -e...] foo  hello  bye</span>
<span style="color: #a3fefe">[Timer] 3 seconds</span></code>
</pre>

`none` leaves all control characters in place. In this case, we will apply both the text coloring (`\033[35m`) and text overwriting (`\033[3D`).
<pre>
<code><span style="color: #ff79c6">$</span><span> shrun -lx10 --cmd-log-strip-control none "echo -e ' foo \033[35m hello \033[3D bye '; sleep 5"</span>
<span style="color:">[Command][echo -e...] foo <span style="color: magenta"> hel bye</span></span>
<span style="color: #a3fefe">[Timer] 3 seconds</span></code>
</pre>

`smart` removes the control chars but leaves the text coloring, so we will have the magenta text but not overwriting.
<pre>
<code><span style="color: #ff79c6">$</span><span> shrun -lx10 --cmd-log-strip-control smart "echo -e ' foo \033[35m hello \033[3D bye '; sleep 5"</span>
<span style="color:">[Command][echo -e...] foo <span style="color: magenta"> hello  bye</span</span>
<span style="color: #a3fefe">[Timer] 3 seconds</span></code>
</pre>

### File Log Strip Control

**Arg:** `-f, --file-log-strip-control <all | smart | none>`

**Description**: Like [`--cmd-log-strip-control`](#strip-control), but applies to file logs. If no option is given, defaults to `all`.

### Command Name Truncation

**Arg:** `-x, --cmd-name-trunc NATURAL`

**Description:** Non-negative integer that limits the length of commands/key-names in the console logs. Defaults to no truncation. This affects everywhere the command/key-name shows up (i.e. in command logs or final success/error message). File logs created via `--file-log` are unaffected.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log --cmd-name-trunc 10 "for i in {1..3}; do echo hi; sleep 1; done"</span>
<span style="color:">[Command][for i i...] hi</span>
<span style="color: #a3fefe">[Timer] 2 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log --cmd-name-trunc 10 "for i in {1..3}; do echo hi; sleep 1; done"</span>
<span style="color: #69ff94">[Success][for i i...] 3 seconds</span>
<span style="color: #d6acff">[Finished] 3 seconds</span></code>
</pre>

### Command Line Truncation

**Arg:** `-y, --cmd-log-line-trunc <NATURAL | detect>`

**Description:** Non-negative integer that limits the length of logs produced via `--cmd-log` in the console logs. Can also be the string literal `detect`, to detect the terminal size automatically. Defaults to no truncation. This does not affect file logs with `--file-log`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log --cmd-log-line-trunc 80 "echo 'some ridiculously long command i mean is this really necessary' && sleep 5"</span>
<span style="color:">[Command][echo 'some ridiculously long command i mean is this really necessar...</span>
<span style="color: #a3fefe">[Timer] 3 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log --cmd-log-line-trunc detect "echo 'some ridiculously long command i mean is this really necessary' && sleep 5"</span>
<span style="color:">[Command][echo 'some ridiculously long command i mean is this really necessary' && sleep 5] some ridiculously long command...</span>
<span style="color: #a3fefe">[Timer] 3 seconds</span></code>
</pre>

## Miscellaneous

### Default Config

**Arg:** `--default-config`

**Description:** Writes a default configuration to `stdout`.

# Building

## Prerequisites

You will need one of:

* [cabal-install 2.4+](https://www.haskell.org/cabal/download.html) and:
  * [ghc 9.2](https://www.haskell.org/ghcup/)
  * [ghc 9.4](https://www.haskell.org/ghcup/)
* [nix](https://nixos.org/download.html)

If you have never built a haskell program before, `cabal` + `ghcup` is probably the best choice.

## Cabal

You will need `ghc` and `cabal-install`. From there `shrun` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Nix

### From source

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `shrun` can be built with `nix build`, which will compile and run the tests.

To launch a shell with various tools (e.g. `cabal`, `hls`), run `nix develop`. After that we can launch a repl with `cabal repl` or run the various tools on our code. At this point you could also build via `cabal`, though you may have to first run `cabal update`. This will fetch the needed dependencies from `nixpkgs`.

### Via nix

Because `shrun` is a flake, it be built as part of a nix expression. For instance, if you want to add `shrun` to `NixOS`, your `flake.nix` might look something like:

```nix
{
  description = "My flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    shrun-src.url= "github:tbidne/shrun/main";
  };

  outputs = { self, nixpkgs, shrun-src, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = system;
      };
      shrun = shrun-src.packages."${system}".default;
      # Alternative if you want tests disabled.
      #shrun = pkgs.haskell.lib.dontCheck shrun-src.packages."${system}".default;
    in
    {
      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          system = system;
          modules = [
            (import ./configuration.nix { inherit pkgs shrun; })
          ];
        };
      };
    };
}
```

Then in `configuration.nix` you can simply have:

```nix
{ pkgs, shrun, ... }:

{
  environment.systemPackages = [
    shrun
  ];
}
```
