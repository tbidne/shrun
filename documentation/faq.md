# FAQ

---

### Table of Contents

- [If I don't run multiple commands all that often, does shrun hold any value?](#if-i-dont-run-multiple-commands-all-that-often-does-shrun-hold-any-value)
- [What if a command needs sudo?](#what-if-a-command-needs-sudo)
- [What if my command relies on interactive shell?](#what-if-my-command-relies-on-interactive-shell)
- [Init vs. Legend?](#init-vs-legend)
- [Can file logging preserve formatting?](#can-file-logging-preserve-formatting)
- [How do I set bash auto-completions?](#how-do-i-set-bash-auto-completions)

## If I don't run multiple commands all that often, does shrun hold any value?

`shrun` offers several advantages over running raw commands, beyond concurrency.

- Desktop notifications allow us to "fire-and-forget" commands. That is, we can run a command in one window, switch windows/desktops to do something else, then be notified when the command finishes. Otherwise we have to manually switch back to check if the command finished, which is mildly annoying.

- Automatic file logging often makes debugging failures easier. It is usually easier searching a file for relevant logs, as opposed to scrolling (possibly large) terminal output. The `--file-log-delete-on-success` option makes this workflow even nicer, as we do not leave log files around unless something goes wrong.

- Having a running timer is useful when we have some idea how long a command _should_ take. Not to mention it is psychologically reassuring 🙂.

## What if a command needs sudo?

Commands _can_ receive `stdin`, so running e.g. `shrun "sudo some command"` will launch the sudo prompt. From there we can type the password and hit `enter` as usual.

However this is a bit clunky as the timer text will overwrite the `[sudo] password for ...` prompt, and multiple commands add more complication.

It is therefore easiest to run sudo first to elevate privileges, then execute `shrun` as normal e.g.

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
[Finished] 0 seconds
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

## Can file logging preserve formatting?

In general, we would like `shrun`'s file logging to preserve command log formatting when possible. For example, `shrun`'s test suite prints output like:

```
Test suite unit: RUNNING...
Unit tests
  Shrun.Configuration.Args.Parsing
    Defaults
      Parses default args:                                               OK
          ✓ testDefaultArgs passed 1 test.
    --config
      Parses -c:                                                         OK
          ✓ testConfigShort passed 1 test.
      Parses --config:                                                   OK
          ✓ testConfig passed 1 test.
...
```

We would therefore like `shrun` to log something like:

```
[2024-06-03 17:48:13][Command][cabal test unit] Test suite unit: RUNNING...
[2024-06-03 17:48:13][Command][cabal test unit] Unit tests
[2024-06-03 17:48:13][Command][cabal test unit]   Shrun.Configuration.Args.Parsing
[2024-06-03 17:48:13][Command][cabal test unit]     Defaults
[2024-06-03 17:48:13][Command][cabal test unit]       Parses default args:                                               OK
[2024-06-03 17:48:13][Command][cabal test unit]           ✓ testDefaultArgs passed 1 test.
[2024-06-03 17:48:13][Command][cabal test unit]     --config
[2024-06-03 17:48:13][Command][cabal test unit]       Parses -c:                                                         OK
[2024-06-03 17:48:13][Command][cabal test unit]           ✓ testConfigShort passed 1 test.
[2024-06-03 17:48:13][Command][cabal test unit]       Parses --config:                                                   OK
[2024-06-03 17:48:13][Command][cabal test unit]           ✓ testConfig passed 1 test.
...
```

It is easy to split logs on newlines and log each line separately, but there are still complications. The fundamental problem is that we are reading `N` bytes of data at a time, so there is no guarantee that our read will end at a newline. We thus have to handle this case ourselves. To that end, we introduce several options that interact with command-log reading:

- `--command-log-poll-interval`: How fast `shrun` reads logs from the underlying commands.
- `--command-log-read-size`: Maximum number of bytes `shrun` will read from the underlying command, in a single read.
- `--command-log-read-strategy`: The first strategy, `block`, simply reads and logs `N` bytes at a time. The more complex `block-line-buffer` also reads `N` bytes, however, it buffers logs until a newline is found, or some threshold is exceeded.
- `--command-log-buffer-length`: Used in conjunction with `block-line-buffer`. If the length is exceeded, the buffer is flushed, to avoid holding an arbitrarily large string in memory.
- `--command-log-buffer-timeout`: Same idea as `--command-log-buffer-length`, except the threshold is a timeout.

The general hope is that logs are newline-terminated and `--command-log-read-size` is large enough to read whatever the underlying command is logging, so we will not end up cutting anything off. Then we can split the logs on newlines and log each line separately. Even so, there are a couple ways the intended formatting can be disrupted:

- If the `--command-log-poll-interval` is slower than the underlying command's logging, there will be a build-up of logs in the next read, so it is possible the total size is greater than `--command-log-read-size`, hence we will be cutting off logs at an arbitrary place.
- On the other hand, if the `--command-log-poll-interval` is _faster_, it is possible to break up an "incomplete log". For instance, our test examples prints the text description like `Parses default args:` immediately, then only prints the remaining `...OK` after the test finishes. Thus we might read the first part of the log without its corresponding end, and the log will be broken.

The `block-line-buffer` strategy is the primary solution to these problems, and indeed the reason this option was introduced.

> [!WARNING]
>
> The `block-line-buffer` strategy can lead to nonsense file logs when there are multiple commands. Hence this is disallowed.

With that out of the way, we can now justify the default behavior.

- When we have exactly one command and/or `file-logging` is disabled, we use the `block-line-buffer` strategy. This has the best chance at preserving formatting, but it can lead to nonsense file logs when there are multiple commands.
- Otherwise (multiple command and `file-logging` enabled), we use the `block` strategy.

> [!TIP]
>
> There is little reason to explicitly set `--read-strategy block-line-buffer` manually, as the only cases that is permissible (single command and/or file-logging is disabled), `shrun` will automatically choose that strategy. Thus the only reason is to be explicit.

There are various other tweaks one can try if the file log formatting is still damaged e.g. increasing `--command-log-buffer-(length|timeout)` and/or `--command-log-read-size`. Decreasing the `--command-log-poll-interval` _could_ help, though -- as we see from the description above -- this is not a general solution, and it may push the CPU usage unacceptably high regardless, so it is likely not a good solution.

If none of those help, the best solution is likely to simply use `--command-log-read-strategy block` -- which generally does a pretty good job -- and make your peace with the fact that this is all best-effort 🙂.

## How do I set bash auto-completions?

To get bash auto-completions for `shrun` options, add the following to your `.bashrc` (assuming `shrun` is on the `PATH`):

```sh
. <(shrun --bash-completion-script `which shrun`)
```
