# FAQ

---

### Table of Contents

- [If I don't run multiple commands all that often, does shrun hold any value?](#if-i-dont-run-multiple-commands-all-that-often-does-shrun-hold-any-value)
- [What if a command needs sudo?](#what-if-a-command-needs-sudo)
- [What if my command relies on interactive shell?](#what-if-my-command-relies-on-interactive-shell)
- [Init vs. Legend?](#init-vs-legend)
- [How do I run sequential commands?](#how-do-i-run-sequential-commands)
- [Can file logging preserve formatting?](#can-file-logging-preserve-formatting)
- [How do I set shell auto-completions?](#how-do-i-set-shell-auto-completions)
- [What does the status mean?](#what-does-the-status-mean)
- [What does too many similar notifications mean?](#what-does-too-many-similar-notifications-mean)

## If I don't run multiple commands all that often, does shrun hold any value?

`shrun` offers several advantages over running raw commands, beyond concurrency.

- Desktop notifications allow us to "fire-and-forget" commands. That is, we can run a command in one window, switch windows/desktops to do something else, then be notified when the command finishes. Otherwise we have to manually switch back to check if the command finished, which is mildly annoying.

- Automatic file logging often makes debugging failures easier. It is usually easier searching a file for relevant logs, as opposed to scrolling (possibly large) terminal output. The `--file-log-delete-on-success` option makes this workflow even nicer, as we do not leave log files around unless something goes wrong.

- Having a running timer is useful when we have some idea how long a command _should_ take. Not to mention it is psychologically reassuring 🙂.

## What if a command needs sudo?

In general, `stdin` is ignored so commands that require `stdin` will not work properly. That said, `sudo` _does_ work i.e. launches the `sudo` prompt. From there we can type the password and hit `enter` as usual.

However this is a bit clunky as the timer text will overwrite the `[sudo] password for ...` prompt, and multiple commands add more complication.

It is therefore easiest to run `sudo` first to elevate privileges, then execute `shrun` as normal e.g.

```sh
# Run sudo with some dummy command
$ sudo ls
...

# shrun can now execute sudo without requiring stdin
$ shrun ...
```

## What if my command relies on interactive shell?

`shrun` executes shell commands non-interactively, which means we do not have access to anything defined in, say, `~/.bashrc` or `~/.bash_profile`. This can be annoying if we want to run any of these functions/aliases.

```sh
# ~/.bashrc
foo () {
  ...
}

bar () {
  ...
}
```

```sh
$ shrun foo bar
[Error][bar] 0 seconds: /bin/sh: line 1: bar: command not found
[Error][foo] 0 seconds: /bin/sh: line 1: foo: command not found
[Finished][0|0|2|0] 0 seconds
```

Fortunately, the [`init`](configuration.md#init) option exists exactly for this purpose:

```sh
$ shrun --init ". ~/.bashrc" foo bar
```

This is equivalent to running:

```sh
$ shrun ". ~/.bashrc && foo" ". ~/.bashrc && bar"
```

> [!TIP]
>
> An extensive `~/.bashrc` may contain code that does not work well when loading non-interactively e.g. the common idiom `[[ $- == *i* ]] || return` will cause `shrun` to choke. Instead, you may want to create a file for your functions e.g. `~/.bash_functions.sh`, source _that_ in `~/.bashrc`, and then use it with `shrun` instead:
>
> ```sh
> $ shrun --init ". ~/.bash_functions.sh" ...
> ```

## Init vs. Legend?

There are two ways to use command aliases with `shrun`. One is with the toml file's [`legend`](./configuration.md#legend) section:

```toml
legend = [
  { key = 'backend', val = 'javac ...' },
]
```

```sh
# runs javac as a shrun command
$ shrun -c config.toml backend
```

Another is with [`init`](configuration.md#init):

```sh
# e.g. define as bash alias/function instead in ~/.bashrc or wherever
backend () { javac ...; }
```

```sh
# runs the bash 'backend' function as a shrun command
$ shrun --init ". ~/.bashrc" backend
```

Why two methods?

1. The first reason is historical: `legend` preceded `init`, so for a time the former was the only way to use aliases with `shrun`.

2. More importantly, the legend allows us to easily combine _multiple_ commands and keep `shrun`'s usual semantics (e.g. concurrency, independence):

    ```toml
    legend = [
      { key = 'backend', val = 'javac ...' },
      { key = 'ui', val = 'npm run build' },
      { key = 'all', val = ['backend', 'ui'] },
    ]
    ```

    ```sh
    # runs ui and backend concurrently
    $ shrun -c config.toml all
    ```

    On the other hand, the naive bash translation has different semantics:

    ```sh
    backend () { javac ...; }

    ui () { npm run build; }

    all () { backend; ui; }
    ```

    ```sh
    # runs the bash 'all' function as a shrun command, so backend and ui are
    # _not_ run concurrently / separately!
    $ shrun --init ". ~/.bashrc" all
    ```

    Of course you _can_ write concurrent bash code. But the problem of running multiple commands was in fact `shrun`'s [motivating example](../README.md#motivation).

Thus the conclusion is: If you have a single alias that you may want global (e.g. called with or without `shrun`), and you do not envision regularly running that alias simultaneously with other commands, by all means, throw it in e.g. `~/.bashrc` and use `init`.

If, instead, you don't want the alias in `~/.bashrc` or you regularly run it with some other commands, consider putting it in the toml's `legend`.

> [!TIP]
>
> You can also split the difference here. Put the individual commands `backend` and `ui` in `~/.bashrc`, load that with `init`, and put the aggregate `all` command in `legend`. This allows using `backend` and `ui` independent of `shrun`, while retaining `shrun`'s advantages with `all`:
>
> ```sh
> $ shrun --init ". ~/.bashrc" -c config.toml all
> ```

## How do I run sequential commands?

We sometimes want to run commands that depend on one another e.g. only run `cmd2` after `cmd1` successfully finishes. In bash, the usual pattern for this is `&&`: `cmd1 && cmd2`.

As `shrun`'s original raison d'être was to run (independent) commands concurrently, such dependencies were not supported.

```
$ shrun cmd1 cmd2 # runs both concurrently, not what we want!
```

The workaround would be to use `&&` manually e.g. `shrun "cmd1 && cmd2"` or `shrun cmd1 && shrun cmd2`. This works, but it means we lose the benefits of having `shrun` manage individual commands (logging, notifications). It is especially annoying if we have several commands that can all be run concurrently except for one, which spoils the whole thing.

The `--edges` option is introduced for this reason. It allows us to specify dependencies between commands via a numeric index, which is based on the command's left-to-right appearance in the CLI. For example, the above scenario would be run as:

```sh
$ shrun --edges "1 & 2" cmd1 cmd2
```

This declares that the second command should be run only after the first command successfully finishes. If it fails, then the command will not be run at all.

We also provide `or`-edges (`||`) and `any`-edges (`;`):

|     | Syntax |   Bash equivalent | Description                      |
|:----|-------:|------------------:|:---------------------------------|
| And |    `&` |    `cmd1 && cmd2` | Runs `cmd2` iff `cmd1` succeeds. |
| Or  |   `\|` |  `cmd1 \|\| cmd2` | Runs `cmd2` iff `cmd1` fails.    |
| Any |    `;` |     `cmd1 ; cmd2` | Runs `cmd2` iff `cmd1` finishes. |

For example:

```sh
# Runs cmd2 if cmd1 fails; runs cmd3 after cmd1 finishes.
$ shrun --edges "1 | 2, 1 ; 3" cmd1 cmd2 cmd3
```

We allow arbitrarily many comma-separated dependencies, including some syntactic sugar:

|                 |                                   Syntax |                                            Desugaring |
|:----------------|-----------------------------------------:|------------------------------------------------------:|
| Multi-edge-sets |                        `{1, 2} & {3, 4}` |                          `1 & 3, 1 & 4, 2 & 3, 2 & 4` |
| Extended edges  |                             `1 & 4 \| 5` |                                       `1 & 4, 4 \| 5` |
| Set ranges      |                            `{1, 3 .. 5}` |                                        `{1, 3, 4, 5}` |
| Edge ranges     | `1 &.. 3` <br> `1 \|.. 3` <br> `1 ;.. 3` |       `1 & 2 & 3` <br> `1 \| 2 \| 3` <br> `1 ; 2 ; 3` |

For instance:

```sh
$ shrun --edges "{1,2..4} & 7 &.. 9 & {10, 11}, 12 & 13 & 16" cmd1 cmd2 ... cmd16

# The above is equivalent to:
$ shrun --edges "
  1 & 7, 2 & 7, 3 & 7, 4 & 7,
  7 & 8, 8 & 9,
  9 & 10, 9 & 11,
  12 & 13, 13 & 16" cmd1 cmd2 ... cmd16
```

This means:

- Commands 1, 2, 3, and 4 will start immediately.
  - Command 7 will start once 1, 2, 3, and 4 finish successfully.
  - Command 8 will start once 7 finishes successfully.
  - Command 9 will start once 8 finishes successfully.
  - Commands 10 and 11 will start once 9 finishes successfully.
- Command 12 will start immediately.
  - Command 13 will start once 12 finishes successfully.
  - Command 16 will start once 13 finishes successfully.
- Command 14 will start immediately.
- Command 15 will start immediately.

We also allow the literals `&&`, `||`, and `;;`, which declares all commands will be run sequentially with the given edge. That is,

```sh
$ shrun --edges "&&" cmd1 cmd2 ... cmdn

# The above is equivalent to:
$ shrun --edges "1 &.. n" cmd1 cmd2 ... cmdn
```

- Command 1 will start immediately.
- Command 2 will start once 1 succeeds.
- ...
- Command n will start once n-1 succeeds.

> [!IMPORTANT]
>
> There are some nuances.
>
> - Edges respect aliases. That is, suppose we have
>
>     ```toml
>     legend = [ { key = 'all', val = ['cmd2', 'cmd3', 'cmd4'], edges = '1 & 3' } ]
>     ```
>
>   Then
>
>     ```sh
>     $ shrun -c config.toml cmd1 all cmd5 --edges "1 & 2, 2 & 3"
>     ```
>
>   will be expanded to
>
>     ```sh
>                all
>            ┌────┴────┐
>      (1)  (2)  (3)  (4)  (5)
>     cmd1 cmd2 cmd3 cmd4 cmd5
>     ```
>
>   and the edges will therefore be
>
>     ```sh
>     # Original '1 & 2' edge i.e. "cmd1" & "all"
>     1 & 2, 1 & 3, 1 & 4,
>     # all's '1 & 3' edge i.e. "cmd2 & cmd4"
>     2 & 4,
>     # Original '2 & 3' edge i.e. "all" & "cmd5"
>     2 & 5, 3 & 5, 4 & 5
>     ```
>
>   That is, edges are mapped based on alias expansion, and if an edge refers
>   to an alias, it is taken to refer to _every_ command in that alias.
>
> - Dependencies must be "well-behaved" e.g. all vertices must exist, be
>   reachable, and there must be no cycles.

## Can file logging preserve formatting?

The option `--command-log-read-strategy block-line-buffer` attempts to preserve output formatting in file logs. The option is on by default *except* when all of the following are true:

- There are concurrent commands.
- `file-log: on`
- `file-log-multi: off`

This is because having multiple commands writing to the same log file with `block-line-buffer` can produce nonsense, and is exactly why `file-log-multi` exists, to give each command its own log file. Hence if you are want `block-line-buffer` with concurrent commands, set `file-log-multi: on`.

> [!TIP]
>
> You can set `file-log.multi = "auto"` in the toml config and forget all about this, as this will always have `block-line-buffer = "on"` and do the right thing automatically:
>
> - If there are concurrent commands, `multi == "on"`.
> - Otherwise `multi == "off"`.

## How do I set shell auto-completions?

Shrun supports tab-completions for bash, fish, and zsh. To load them, run the appropriate script:

```sh
$ source <(shrun --bash-completion-script `which shrun`)
$ source <(shrun --fish-completion-script `which shrun`)
$ source <(shrun --zsh-completion-script `which shrun`)
```

Furthermore, we can use the `--legend-keys-cache` option to save legend keys, so that we get tab completions on the next run.

```sh
# some_alias is a legend key in config.toml. The first time we use
# config.toml, we have to fully type it out.
$ shrun --legend-keys-cache add --config config.toml some_alias

# Now that the first run saved the keys, we can use tab completions.
$ shrun --config config.toml some<TAB> # will auto-complete to some_alias
```

The keys will be persisted until `--legend-keys-cache clear` is used (or overwritten with `--legend-keys-cache write`). Hence `--legend-keys-cache add` only needs to be run the first time a particular legend file is used, though it does not hurt to set it in the toml config.

## What does the status mean?

The status bar e.g.

```sh
$ shrun --edges "1 & 3, 2 & 3" cmd1 cmd2 cmd3
[Command][cmd1] cmd1 output...
[Command][cmd2] cmd2 output...
[Status][1|2|0|0] 5 seconds
```

refers to the number of tasks in each status i.e.

```
[waiting|running|failed|succeeded]
```

## What does too many similar notifications mean?

Some notification systems e.g. dbus will error if identical notifications are sent too quickly. This can be a problem for identical commands:

```sh
$ shrun "sleep 1" "sleep 1"
[Warn] Could not send notification: sent too many similar notifications.
[Success][sleep 1] 1 second
[Success][sleep 1] 1 second
[Warn] Could not send notification: sent too many similar notifications.
[Finished][0|0|0|2] 1 second
```

We attempt to mitigate this error by intercepting it and turning it into a warning, though it is possible for it to slip by. There are two workarounds:

1. Differentiate identical commands with extraneous whitespace, if possible.

    ```sh
    $ shrun "sleep  1" "sleep 1"
    [Success][sleep 1] 1 second
    [Success][sleep  1] 1 second
    [Finished][0|0|0|2] 1 second
    ```

1. Set `--common-log-command-index on`.

    ```sh
    $ shrun --common-log-command-index on "sleep 1 " "sleep 1"
    [Success][2. sleep 1] 1 second
    [Success][1. sleep 1] 1 second
    [Finished][0|0|0|2] 1 second
    ```
