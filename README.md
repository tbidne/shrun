<div align="center">

# Shell-Run

## Run Shell Commands Concurrently

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/shell-run?include_prereleases&sort=semver)](https://github.com/tbidne/shell-run/releases/)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/shell-run?color=blue)](https://opensource.org/licenses/BSD-3-Clause)


[![nix](https://img.shields.io/github/workflow/status/tbidne/shell-run/nix/main?label=nix%209.2.2&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/shell-run/actions/workflows/nix_ci.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/shell-run/stack/main?label=stack%2019.4&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/shell-run/actions/workflows/stack_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/shell-run/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/shell-run/actions/workflows/style_ci.yaml)

[![8.10.7](https://img.shields.io/github/workflow/status/tbidne/shell-run/8.10.7/main?label=8.10.7&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/shell-run/actions/workflows/ghc_8-10.yaml)
[![9.0.2](https://img.shields.io/github/workflow/status/tbidne/shell-run/9.0.2/main?label=9.0.2&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/shell-run/actions/workflows/ghc_9.0.yaml)
[![9.2.2](https://img.shields.io/github/workflow/status/tbidne/shell-run/9.2.2/main?label=9.2.2&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/shell-run/actions/workflows/ghc_9.2.yaml)

</div>

---

### Table of Contents
- [Motivation](#motivation)
- [Introduction](#introduction)
- [Options](#options)
  - [Core Functionality](#core-functionality)
    - [Legend](#legend)
    - [Timeout](#timeout)
  - [Logging](#logging)
    - [Command Log](#command-log)
    - [File Log](#file-log)
    - [Disable Log](#disable-log)
  - [Log Formatting](#log-formatting)
    - [Key Show](#key-show)
    - [Strip Control](#strip-control)
    - [Command Name Truncation](#command-name-truncation)
    - [Command Line Truncation](#command-line-truncation)
- [Building](#building)
  - [Cabal](#cabal)
  - [Stack](#stack)
  - [Nix](#nix)

# Motivation

`shell-run` was borne of frustration. Suppose you run several shell commands on a regular basis e.g. updates after pulling the latest code. You can run these manually like:

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

1. You do not receive any information about how long your commands have been running. If any of the commands are long-lived, how do you know when it's been "too long" and you should cancel them? You can look at a clock or use a stopwatch, but that requires remembering every time you run the command, which is certainly unsatisfying.

1. These commands are all run synchronously even though there may be no relation between them. For example, if you have three commands that each take 5 minutes, the combination will take 15 minutes. This is usually unnecessary.

1. Related to above, if any command fails then subsequent ones will not be run. This can be frustrating, as you may kick off a run and leave, only to return and find out that later, longer-running commands never ran because of some trivial error in the beginning.

1. It does not scale. Imagine you have variations of `cmd3` you want to run under different circumstances. You could create multiple aliases:

        
        alias run_commands_cmd3a="cmd1 && cmd2 && cmd3a"
        alias run_commands_cmd3b="cmd1 && cmd2 && cmd3b"

    But this is messy and grows exponentially in the number of aliases for each variation.

`shell-run` purports to overcome these limitations.

# Introduction

In a nut-shell (😉), `shell-run` is a wrapper around running shell commands. For instance:

```sh
shell-run "some long command" "another command"
```

Will run `some long command` and `another command` concurrently.

A running timer is provided, and stdout/stderr will be updated when a command finishes/crashes, respectively. Example of running two commands (`sign-peace-treaty` and `takeover`) from a custom legend file:

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run -ck -l examples/shell-run.legend sign-peace-treaty takeover</span>
<span style="color: #ff6e6e">[Error] [sign-peace-treaty] /bin/sh: line 1: lol psyche: command not found. Time elapsed: 1 second</span>
<span style="color: #69ff94">[Info] [querying-targets] Success. Time elapsed: 2 seconds</span>
<span style="color:">[Command] [skynet] preparing nuclear missil-- i mean gift baskets</span>
<span style="color:">[Command] [ui] adding emojis. we like to have fun :-)</span>
<span style="color: #a3fefe">[Info] Running time: 6 seconds</span></code>
</pre>

Note: `shell-run` colors its logs, and the examples here _should_ show use these colors. Unfortunately github does not render them, so you will have to view this markdown file somewhere else to see them.

# Options

## Core Functionality

### Legend

**Arg:** `-l, --legend PATH`

**Description**: A legend file can be provided that maps key names to commands. Lines are formatted `<cmd_key>=<command value>` (no angle brackets).

Each line can be separated by as many new lines as desired, and comments start with a `#`. Command values themselves can include multiple commands delimited by two commas, and they may reference other commands.

If this option is omitted, we search for a legend file in the Xdg config directory e.g. `~/.config/shell-run/shell-run.legend`. This will not cause an error if it is not found, though there will be a log message.

**Example:** For instance, given a legend file

```text
cmd1=echo "command one"

# recursive references
cmd2=cmd1
cmd3=cmd2

cmd4=command four

# runs 3 and 4
all=cmd3,,cmd4,,echo hi
```

Then the command

```sh
shell-run --legend=path/to/legend all "echo cat"
```

Will run `echo "command one"`, `command four`, `echo hi` and `echo cat` concurrently. A picture is worth a thousand words:

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --legend=examples/shell-run.legend all "echo cat"</span>
<span style="color: #69ff94">[Info] [echo cat] Success. Time elapsed: 0 seconds</span>
<span style="color: #69ff94">[Info] [echo hi] Success. Time elapsed: 0 second</span>
<span style="color: #69ff94">[Info] [echo "command one"] Success. Time elapsed: 0 second</span>
<span style="color: #ff6e6e">[Error] [command four] Error: '/bin/sh: line 1: four: command not found. Time elapsed: 0 seconds</span>
<span style="color: #d6acff">[Info] Finished! Total time elapsed: 0 seconds</span></code>
</pre>

Note: duplicate keys will cause a parse error to be thrown when loading. Cyclic keys are also disallowed, though these will only throw if you actually try to execute one (i.e. merely having cyclic definitions in the legend file will not throw an error).

### Timeout

**Arg:** `-t, --timeout NATURAL`

**Description:** The provided timeout must be either a raw integer (interpreted as seconds), or a "time string" e.g. `1d2m3h4s`, `3h20s`. All integers must be non-negative. If the timeout is reached, then all remaining commands will be cancelled.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --timeout 8 "sleep 5" "sleep 10" "sleep 15"</span>
<span style="color: #69ff94">[Info] [sleep 5] Success. Time elapsed: 5 seconds</span>
<span style="color: #d3d38e">[Warn] Timed out, cancelling remaining commands: sleep 10, sleep 15</span>
<span style="color: #d6acff">[Info] Finished! Total time elapsed: 9 seconds</span></code>
</pre>

## Logging

### Command Log

**Arg:** `-c, --cmd-log`

**Description:** The default behavior is to swallow logs for the commands themselves. This flag gives each command a console region in which its logs will be printed. Only the latest log per region is shown at a given time.

Note: When commands have complicated output, they logs can interfere with each other (indeed even overwrite themselves). We attempt to mitigate such situations, though see [Strip Control](#strip-control).

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --cmd-log "for i in {1..10}; do echo hi; sleep 1; done"</span>
<span style="color:">[Command] [for i in {1..10}; do echo hi; sleep 1; done] hi</span>
<span style="color: #a3fefe">[Info] Running time: 7 seconds</span></code>
</pre>

vs.

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run "for i in {1..10}; do echo hi; sleep 1; done"</span>
<span style="color: #a3fefe">[Info] Running time: 7 seconds</span></code>
</pre>

Note: Both the commands' `stdout` and `stderr` are treated the same, logged with the same formatting. This is because many shell programs perform redirection like `echo ... >&2` (i.e. redirect `stdout` to `stderr`). Not only does this mean we need to take both if we do not want to skip any output, but it also means it does not make sense to try to differentiate the two anymore, as that information has been lost.

Practically speaking, this does not have much effect, just that if a command dies while `--cmd-log` is enabled, then the final `[Error] ...` output may not have the most relevant information. See [File Log](#file-log) for details on investigating command failure.

### File Log

**Arg:** `-f, --file-log PATH`

**Description**: If a path is supplied, all logs will additionally be written to the supplied file. Furthermore, command logs will be written to the file irrespective of `--cmd-log`. Console logging is unaffected. This can be useful for investigating command failures. If the string literal `default` or `d` is given, we will write to the Xdg config directory e.g. `~/.config/shell-run/shell-run.log`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --file-log=out.log --cmd-log "sleep 2" "bad" "for i in {1..3}; do echo hi; sleep 1; done"</span>
<span style="color: #ff6e6e">[Error] [for i in {1..10}; do echo hi; sleep 1; done] hi</span>
<span style="color: #69ff94">[Info] [sleep 2] Success. Time elapsed: 2 seconds</span>
<span style="color: #69ff94">[Info] [for i in {1..3}; do echo hi; sleep 1; done] Success. Time elapsed: 3 seconds</span>
<span style="color: #d6acff">[Info] Finished! Total time elapsed: 3 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> cat out.log</span>
<span style="color:">[2022-05-26 11:25:59.150635686 UTC] [Command] [for i in {1..3}; do echo hi; sleep 1; done] hi</span>
<span style="color:">[2022-05-26 11:25:59.152213816 UTC] [Command] [bad] /bin/sh: line 1: bad: command not found</span>
<span style="color:">[2022-05-26 11:25:59.152253545 UTC] [Error] [bad] /bin/sh: line 1: bad: command not found. Time elapsed: 0 seconds</span>
<span style="color:">[2022-05-26 11:26:00.151610059 UTC] [Command] [for i in {1..3}; do echo hi; sleep 1; done] hi</span>
<span style="color:">[2022-05-26 11:26:01.150768195 UTC] [Info] [sleep 2] Success. Time elapsed: 2 seconds</span>
<span style="color:">[2022-05-26 11:25:59.150635686 UTC] [Command] [for i in {1..3}; do echo hi; sleep 1; done] hi</span>
<span style="color:">[2022-05-26 11:26:02.153745075 UTC] [Info] Finished! Total time elapsed: 3 seconds</span></code>
</pre>

### Disable Log

**Arg:** `-d, --disable-log`

**Description**: This option globally disables all logging i.e. ordinary logs and those created via `--cmd-log` and `--file-log`. As most uses will want at least the default success/error messages and timers, this option is primarily intended for debugging or testing where logging is undesirable.

## Log Formatting

### Key Show

**Arg:** `-k, --key-show`

**Description:** When displaying logs pertaining to a specific command, the default behavior is to use the actual command as the name. This can make the logs cluttered if the command is long, or it can be confusing if there are multiple similar commands that only have minor syntactic differences. This flag instead uses the key name for display, if one exists.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --key-show --cmd-log --legend=examples/shell-run.legend skynet</span>
<span style="color:">[Command] [skynet] preparing nuclear missil-- i mean gift baskets</span>
<span style="color: #a3fefe">[Info] Running time: 7 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --key-show --cmd-log --legend=examples/shell-run.legend skynet</span>
<span style="color: #69ff94">[Success] [skynet] Success. Time elapsed: 10 seconds</span>
<span style="color: #d6acff">[Info] Finished! Total time elapsed: 10 seconds</span></code>
</pre>

rather than the usual

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --cmd-log --legend=examples/shell-run.legend skynet</span>
<span style="color:">[Command] [echo "preparing nuclear missil-- i mean gift baskets"; sleep 10] preparing nuclear missil-- i mean gift baskets</span>
<span style="color: #a3fefe">[Info] Running time: 7 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --cmd-log --legend=examples/shell-run.legend skynet</span>
<span style="color: #69ff94">[Success] [echo "preparing nuclear missil-- i mean gift baskets"; sleep 10] Success. Time elapsed: 10 seconds</span>
<span style="color: #d6acff">[Info] Finished! Total time elapsed: 10 seconds</span></code>
</pre>

Naturally, this does not affect commands that do not have a key (i.e. those not in a legend file). Also, if the commands are defined recursively, then the key name will be the _final_ key.

### Strip Control

**Arg:** `-s,--strip-control [all | smart | none]`

**Description:** Control characters can wreak layout havoc with the `--cmd-log` option, thus we include this option. The default `all` strips all such chars -- the 'safest' option, as far as layout preservation goes -- though it can leave ugly remnants e.g. ansi escape sequences like `[0m`. `none` does nothing i.e. all chars are left untouched. `smart` attempts to strip only the control chars that affect layout (e.g. cursor movements) and leaves others unaffected (e.g. colors). This has the potential to be the 'prettiest' as:

* Simple formatting is left intact.
* Aforementioned remnants should not be left behind.

Though it is likely to miss some chars. `smart` is experimental and subject to change.

**Example:**

Note: In the following examples, `\033[35m` and `\033[3D` are ansi escape codes. The former sets the text color to magenta, and the latter resets the cursor to the left by 3 places i.e. partially overwrites the previous characters. We also include the options `-cx10` (show command logs and truncate command name to 10 chars) to make the output easier to read.

`all` strips _all_ control characters: `\033` in this case. The means all special formatting / control will be omitted, but the remaining bits of the ansi sequences are left.
<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run -cx10 --strip-control all "echo -e ' foo \033[35m hello \033[3D bye '; sleep 5"</span>
<span style="color:">[Command] [echo -e...] foo [35m hello [3D bye</span>
<span style="color: #a3fefe">[Info] Running time: 3 seconds</span></code>
</pre>

`none` leaves all control characters in place. In this case, we will apply both the text coloring (`\033[35m`) and text overwriting (`\033[3D`).
<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run -cx10 --strip-control none "echo -e ' foo \033[35m hello \033[3D bye '; sleep 5"</span>
<span style="color:">[Command] [echo -e...] foo <span style="color: magenta"> hel bye</span></span>
<span style="color: #a3fefe">[Info] Running time: 3 seconds</span></code>
</pre>

`smart` removes the control chars but leaves the text coloring, so we will have the magenta text but not overwriting.
<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run -cx10 --strip-control smart "echo -e ' foo \033[7m hello \033[3D bye '; sleep 5"</span>
<span style="color:">[Command] [echo -e...] foo <span style="color: magenta"> hello  bye</span</span>
<span style="color: #a3fefe">[Info] Running time: 3 seconds</span></code>
</pre>

### Command Name Truncation

**Arg:** `-x, --cmd-name-trunc NATURAL`

**Description:** Non-negative integer that limits the length of commands/key-names in the console logs. Defaults to no truncation. This affects everywhere the command/key-name shows up (i.e. in command logs or final success/error message). File logs created via `--file-log` are unaffected.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --cmd-log --cmd-name-trunc 10 "for i in {1..3}; do echo hi; sleep 1; done"</span>
<span style="color:">[Command] [for i i...] hi</span>
<span style="color: #a3fefe">[Info] Running time: 2 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --cmd-log --cmd-name-trunc 10 "for i in {1..3}; do echo hi; sleep 1; done"</span>
<span style="color: #69ff94">[Success] [for i i...] Success. Time elapsed: 3 seconds</span>
<span style="color: #d6acff">[Info] Finished! Total time elapsed: 3 seconds</span></code>
</pre>

### Command Line Truncation

**Arg:** `-y, --cmd-line-trunc NATURAL or detect/d`

**Description:** Non-negative integer that limits the length of logs produced via `--cmd-log` in the console logs. Can also be the string literal `detect` or `d`, to detect the terminal size automatically. Defaults to no truncation. This does not affect file logs with `--file-log`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --cmd-log --cmd-line-trunc 80 "echo 'some ridiculously long command i mean is this really necessary' && sleep 5"</span>
<span style="color:">[Command] [echo 'some ridiculously long command i mean is this really ne...</span>
<span style="color: #a3fefe">[Info] Running time: 3 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shell-run --cmd-log --cmd-line-trunc detect "echo 'some ridiculously long command i mean is this really necessary' && sleep 5"</span>
<span style="color:">[Command] [echo 'some ridiculously long command i mean is this really necessary' && sleep 5] some ridiculously long command...</span>
<span style="color: #a3fefe">[Info] Running time: 3 seconds</span></code>
</pre>

# Building

## Prerequisites

You will need one of:

* [cabal-install 2.4+](https://www.haskell.org/cabal/download.html) and one of:
  * [ghc 8.10.7](https://www.haskell.org/ghc/download_ghc_8_10_7.html)
  * [ghc 9.0.2](https://www.haskell.org/ghc/download_ghc_9_0_2.html)
  * [ghc 9.2.2](https://www.haskell.org/ghc/download_ghc_9_2_2.html)
* [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* [nix](https://nixos.org/download.html)

If you have never built a haskell program before, `stack` is probably the best choice.

## Cabal

You will need `ghc` and `cabal-install`. From there `shell-run` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Stack

Like `cabal`, `shell-run` can be built locally or installed globally (e.g. `~/.local/bin/`) with `stack build` and `stack install`, respectively.

## Nix

### From source

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `shell-run` can be built with `nix build`, which will compile and run the tests.

To launch a shell with various tools (e.g. `cabal`, `hls`), run `nix develop`. After that we can launch a repl with `cabal repl` or run the various tools on our code. At this point you could also build via `cabal`, though you may have to first run `cabal update`. This will fetch the needed dependencies from `nixpkgs`.

### Via nix

Because `shell-run` is a flake, it be built as part of a nix expression. For instance, if you want to add `shell-run` to `NixOS`, your `flake.nix` might look something like:

```nix
{
  description = "My flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    shell-run-src.url= "github:tbidne/shell-run/main";
  };

  outputs = { self, nixpkgs, shell-run-src, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = system;
      };
      shell-run = shell-run-src.defaultPackage.${system};
      # Alternative if you want tests disabled.
      #shell-run = pkgs.haskell.lib.dontCheck shell-run-src.defaultPackage.${system};
    in
    {
      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          system = system;
          modules = [
            (import ./configuration.nix { inherit pkgs shell-run; })
          ];
        };
      };
    };
}
```

Then in `configuration.nix` you can simply have:

```nix
{ pkgs, shell-run, ... }:

{
  environment.systemPackages = [
    shell-run
  ];
}
```
