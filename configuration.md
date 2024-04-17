# Configuration

### Table of Contents
  - [Core Functionality](#core-functionality)
    - [Config](#config)
    - [Timeout](#timeout)
    - [Init](#init)
  - [Logging](#logging)
    - [Command Log](#command-log)
    - [Poll Interval](#poll-interval)
    - [Command Log Read Size](#command-log-read-size)
    - [File Log](#file-log)
    - [File Log Mode](#file-log-mode)
    - [File Log Size Mode](#file-log-size-mode)
  - [Log Formatting](#log-formatting)
    - [Timer Format](#timer-format)
    - [Key Hide](#key-hide)
    - [Strip Control](#strip-control)
    - [File Log Strip Control](#file-log-strip-control)
    - [Command Name Truncation](#command-name-truncation)
    - [Command Line Truncation](#command-line-truncation)
  - [Notifications](#notifications)
    - [Notify Action](#notify-action)
    - [Notify System](#notify-system)
    - [Notify Timeout](#notify-timeout)
  - [Miscellaneous](#miscellaneous)
    - [Default Config](#default-config)

---

`shrun` can be configured by either CLI args or a `toml` config file. Most arguments exist in both formats -- where they have the same name -- though some exist only as CLI args. The following describes the CLI args. See [default.toml](./examples/default.toml) for a description of the `toml` file.

> [!TIP]
>
> In general, each option `--foo` has a `--no-foo` variant that disables CLI and toml configuration for that field. For example, the `--no-cmd-log` option will instruct shrun to ignore both CLI `--cmd-log` and toml `cmd-log`, ensuring the default behavior is used (i.e. no command logging).


## Core Functionality

### Config

**Arg:** `-c, --config PATH`

**Description**: Path to TOML config file. If this argument is not given we automatically look in the XDG config directory e.g. `~/.config/shrun/config.toml`. The `--no-config` option disables `--config` and the automatic XDG lookup.

Examples can be found in [examples](./examples).

#### Legend

In addition to providing an alternative to CLI args, the config file has a `legend` section. This allows us to define aliases for commands. Each alias has a key and a value. The value can either be a single unit or a list of units, where a unit is either a command literal (e.g. bash expression) or a recursive reference to another alias.

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
$ shrun --config=path/to/config all "echo cat"
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

> [!CAUTION]
> Duplicate keys will cause a parse error to be thrown when loading. Cyclic keys are also disallowed, though these will only throw if you actually try to execute one (i.e. merely having cyclic definitions in the legend will not throw an error).

### Timeout

**Arg:** `-t, --timeout (NATURAL | STRING)`

**Description:** The provided timeout must be either a raw integer (interpreted as seconds), or a "time string" e.g. `1d2m3h4s`, `3h20s`. All integers must be non-negative. If the timeout is reached, then all remaining commands will be cancelled.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --timeout 8 "sleep 5" "sleep 10" "sleep 15"</span>
<span style="color: #69ff94">[Success][sleep 5] 5 seconds</span>
<span style="color: #d3d38e">[Warn] Timed out, cancelling remaining commands: sleep 10, sleep 15</span>
<span style="color: #d6acff">[Finished] 9 seconds</span></code>
</pre>

### Init

**Arg:** `-i,--init STRING`

**Description:** If given, `init` is run before each command. That is, `shrun --init "some logic" foo bar` is equivalent to `shrun "some logic && foo" "some logic && bar"`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --init ". examples/bashrc" bash_function</span>
<span style="color: #69ff94">[Success][bash_function] 0 seconds</span>
<span style="color: #d6acff">[Finished] 0 seconds</span></code>
</pre>

vs.

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun bash_function</span>
<span style="color: #ff6e6e">[Error][bash_function] 0 seconds: /bin/sh: line 1: bash_function: command not found</span>
<span style="color: #d6acff">[Finished] 0 seconds</span></code>
</pre>

## Logging

### Command Log

**Arg:** `-l, --cmd-log`

**Description:** The default behavior is to swallow logs for the commands themselves. This flag gives each command a console region in which its logs will be printed. Only the latest log per region is shown at a given time.

> [!NOTE]
> When commands have complicated output, the logs can interfere with each other (indeed even overwrite themselves). We attempt to mitigate such situations: see [Strip Control](#strip-control).

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

> [!NOTE]
> Both the commands' `stdout` and `stderr` are treated the same, logged with the same formatting. This is because many shell programs perform redirection like `echo ... >&2` (i.e. redirect `stdout` to `stderr`). Not only does this mean we need to take both if we do not want to skip any output, but it also means it does not make sense to try to differentiate the two anymore, as that information has been lost.
>
> Practically speaking, this does not have much effect, just that if a command dies while `--cmd-log` is enabled, then the final `[Error] ...` output may not have the most relevant information. See [`--file-log`](#file-log) for details on investigating command failure.

### Poll Interval

**Arg:** `-p, --poll-interval NATURAL`

**Description:** Non-negative integer used in conjunction with [`--cmd-log`](#command-log) and [`--file-log`](#file-log) that determines how quickly we poll commands for logs, in microseconds. A value of 0 is interpreted as infinite i.e. limited only by the CPU. Defaults to 10,000.

> [!WARNING]
> Note that lower values will increase CPU usage. In particular, 0 will max out a CPU thread.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --poll-interval 100 --cmd-log "for i in {1..10}; do echo hi; sleep 1; done"</span>
<span style="color:">[Command][for i in {1..10}; do echo hi; sleep 1; done] hi</span>
<span style="color: #a3fefe">[Timer] 7 seconds</span></code>
</pre>

### Command Log Read Size

**Arg:** `--cmd-log-read-size NATURAL`

**Description:** Non-negative integer that determines that max number of bytes in a single read when streaming command logs ([`--cmd-log`](#command-log) and [`--file-log`](#file-log)). Logs larger than `--cmd-log-read-size` will be read in a subsequent read, hence broken across lines. The default is 1024.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log-read-size 5 "echo abcdef" </span>
<span style="color:">[Success][sleep 1 && e...] abcde</span>
<span style="color: #a3fefe">[Timer] 1 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --cmd-log-read-size 5 "echo abcdef" </span>
<span style="color:">[Success][sleep 1 && e...] f</span>
<span style="color: #a3fefe">[Timer] 2 seconds</span></code>
</pre>

### File Log

**Arg:** `-f, --file-log (default | PATH)`

**Description**: If a path is supplied, all logs will additionally be written to the supplied file. Furthermore, command logs will be written to the file irrespective of [`--cmd-log`](#command-log). Console logging is unaffected. This can be useful for investigating command failures. If the string `default` is given, then we write to the XDG state directory e.g. `~/.local/state/shrun/shrun.log`.

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

**Arg:** `--file-log-mode (append | write)`

**Description:** Mode in which to open the log file. Defaults to write.

### File Log Size Mode

**Arg:** `--file-log-size-mode (warn SIZE | delete SIZE | nothing)`

**Description:** Sets a threshold for the file log size, upon which we either print a warning or delete the file, if it is exceeded. The `SIZE` should include the value and units e.g. `warn 10 mb`, `warn 5 gigabytes`, `delete 20.5B`. Defaults to warning at `50 mb`. Can be disabled with "nothing".

**Exmaple:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --file-log size_mode_warn.log --file-log-size-mode "warn 1 b" "sleep 2"</span>
<span>Warning: log file 'size_mode_warn.log' has size: 11.00 b, but specified threshold is: 1.00 b.</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #d6acff">[Finished] 2 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --file-log size_mode_warn.log --file-log-size-mode nothing "sleep 2"</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #d6acff">[Finished] 2 seconds</span></code>
</pre>

## Log Formatting

### Timer Format

**Arg:** `--timer-format (digital_compact | digital_full | prose_compact | prose_full)`

**Description:** How to format the timer. Defaults to `prose_compact` e.g. `2 hours, 3 seconds`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --timer-format digital_compact --cmd-log "sleep 2"</span>
<span style="color:">[Command][sleep 2] hi</span>
<span style="color: #a3fefe">[Timer] 01</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --timer-format digital_full --cmd-log "sleep 2"</span>
<span style="color:">[Command][sleep 2] hi</span>
<span style="color: #a3fefe">[Timer] 00:00:00:01</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --timer-format prose_compact --cmd-log "sleep 2"</span>
<span style="color:">[Command][sleep 2] hi</span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --timer-format prose_full --cmd-log "sleep 2"</span>
<span style="color:">[Command][sleep 2] hi</span>
<span style="color: #a3fefe">[Timer] 0 days, 0 hours, 0 minutes, 1 second</span></code>
</pre>

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

**Arg:** `-s,--cmd-log-strip-control (all | smart | none)`

**Description:** Control characters can wreak layout havoc with the [`--cmd-log`](#command-log) option, thus we include this option. `all` strips all such chars. `none` does nothing i.e. all chars are left untouched. The default `smart` attempts to strip only the control chars that affect layout (e.g. cursor movements) and leaves others unaffected (e.g. colors). This has the potential to be the 'prettiest' as:

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

**Arg:** `-f, --file-log-strip-control (all | smart | none)`

**Description**: Like [`--cmd-log-strip-control`](#strip-control), but applies to file logs. If no option is given, defaults to `all`.

### Command Name Truncation

**Arg:** `-x, --cmd-name-trunc NATURAL`

**Description:** Non-negative integer that limits the length of commands/key-names in the console logs. Defaults to no truncation. This affects everywhere the command/key-name shows up (i.e. in command logs or final success/error message). File logs created via [`--file-log`](#file-log) are unaffected.

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

**Arg:** `-y, --cmd-log-line-trunc (NATURAL | detect)`

**Description:** Non-negative integer that limits the length of logs produced via [`--cmd-log`](#command-log) in the console logs. Can also be the string literal `detect`, to detect the terminal size automatically. Defaults to no truncation. This does not affect file logs with [`--file-log`](#file-log).

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

## Notifications

These options configure `shrun` to send off desktop notifications for certain actions i.e. a command finishes or shrun itself finishes.

### Notify Action

**Arg:** `--notify-action (final |command | all)`

**Description:** Sends notifications for various actions. `final` sends off a notification when `shrun` itself finishes whereas `command` sends one off each time a command finishes. `all` implies `final` and `command`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --notify-system dbus --notify-action final "sleep 5"</span></code>
</pre>

### Notify System

**Arg:** `--notify-system (dbus | notify-send | apple-script)`

**Description:** The system used for sending notifications. `dbus` and `notify-send` are available on linux, whereas `apple-script` is available for osx.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --notify-system dbus "sleep 5"</span></code>
</pre>

### Notify Timeout

**Arg:** `--notify-timeout (never | NAT)`

**Description:** When to timeout success notifications. Defaults to 10 seconds.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --notify-system dbus --notify-timeout never "sleep 5"</span></code>
</pre>

> [!NOTE]
>
> Timeouts are subject to the whims of the underlying notification system e.g. some notification systems [ignore the timeout entirely](https://bugs.launchpad.net/ubuntu/+source/notify-osd/+bug/390508). Also, "error notifications" (i.e. `shrun` or command failures) are sent with `urgency = critical` where supported, thus may not timeout at all, per [FDO's specification](https://specifications.freedesktop.org/notification-spec/notification-spec-latest.html).

## Miscellaneous

### Default Config

**Arg:** `--default-config`

**Description:** Writes a default configuration to `stdout`.