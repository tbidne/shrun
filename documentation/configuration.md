# Configuration

### Table of Contents
  - [Core Functionality](#core-functionality)
    - [Config](#config)
    - [Command Graph](#command-graph)
    - [Init](#init)
    - [Timeout](#timeout)
  - [Logging](#logging)
    - [Common Logging](#common-logging)
      - [Debug](#debug)
      - [Key Hide](#key-hide)
    - [Command Logging](#command-logging)
      - [Buffer Length](#buffer-length)
      - [Buffer Timeout](#buffer-timeout)
      - [Poll Interval](#poll-interval)
      - [Read Size](#read-size)
      - [Read Strategy](#read-strategy)
    - [Console Logging](#console-logging)
      - [Command Log](#command-log)
      - [Command Name Truncation](#command-name-truncation)
      - [Line Truncation](#line-truncation)
      - [Strip Control](#strip-control)
      - [Timer Format](#timer-format)
    - [File Logging](#file-logging)
      - [File Log](#file-log)
      - [File Command Name Truncation](#file-command-name-truncation)
      - [File Delete On Success](#file-delete-on-success)
      - [File Line Truncation](#file-line-truncation)
      - [File Log Mode](#file-log-mode)
      - [File Log Size Mode](#file-log-size-mode)
      - [File Log Strip Control](#file-log-strip-control)
  - [Notifications](#notifications)
    - [Notify Action](#notify-action)
    - [Notify System](#notify-system)
    - [Notify Timeout](#notify-timeout)
  - [Miscellaneous](#miscellaneous)
    - [Default Config](#default-config)

---

`shrun` can be configured by either CLI args or a `toml` config file. Most arguments exist in both formats -- where they have the same name -- though some exist only as CLI args. The following describes the CLI args. See [config.toml](../examples/config.toml) for a description of the `toml` file.

> [!TIP]
>
> In general, each option `--foo` has a `--no-foo` variant that disables CLI and toml configuration for that field. For example, the `--no-console-log-command` option will instruct `shrun` to ignore both CLI `--console-log-command` and toml `console-log.command`, ensuring the default behavior is used (i.e. no command logging).

> [!NOTE]
>
> `shrun` colors its logs, and the examples shown here _should_ use these colors. Unfortunately github does not render them, so you will have to view this markdown file somewhere else to see them.


## Core Functionality

### Config

**Arg:** `-c, --config PATH`

**Description**: Path to TOML config file. If this argument is not given we automatically look in the XDG config directory e.g. `~/.config/shrun/config.toml`. The `--no-config` option disables `--config` and the automatic XDG lookup.

Examples can be found in [examples](../examples).

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

### Command Graph

**Arg:** `--edges (GRAPH_STR | sequential)`

**Description:** Comma separated list, specifying command dependencies, based on their order. For instance, `--edges '1 -> 3, 2 -> 3'` will require commands 1 and 2 to complete before 3 is run. The literal `sequential` will run all commands sequentially.

**Example:**

<pre>
<code># Normally, the 'sleep 1' command would start and finish first</code>
<code><span style="color: #ff79c6">$</span><span> shrun --init --edges '1 -> 3, 2 -> 3' "sleep 2" "sleep 2" "sleep 1" "sleep 3"</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #69ff94">[Success][sleep 3] 3 seconds</span>
<span style="color: #69ff94">[Success][sleep 1] 1 seconds</span>
<span style="color: #d6acff">[Finished] 3 seconds</span></code>
</pre>

> [!CAUTION]
> Dependencies must be "well-behaved" e.g. all vertices must exist, be reachable, and there must be no cycles.

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

### Timeout

**Arg:** `-t, --timeout (NATURAL | STRING)`

**Description:** The provided timeout must be either a raw integer (interpreted as seconds), or a "time string" e.g. `1d2m3h4s`, `3h20s`. All integers must be non-negative. If the timeout is reached, then all remaining commands will be cancelled.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --timeout 4 "sleep 2" "sleep 6" "sleep 8"</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #d3d38e">[Warn] Timed out</span>
<span style="color: #d3d38e">[Warn] Attempting to cancel:</span>
<span style="color: #d3d38e">  - sleep 6</span>
<span style="color: #d3d38e">  - sleep 8</span>
<span style="color: #d6acff">[Finished] 5 seconds</span></code>
</pre>

## Logging

### Common Logging

This is general logging config.

#### Debug

**Arg:** `--common-log-debug`

**Description:** Enables additional debug logs.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --common-log-debug "sleep 2"</span>
<span style="color:">[Debug][sleep 2] Command: 'ShellCommand "sleep 2"'</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #d6acff">[Finished] 2 seconds</span></code>
</pre>

#### Key Hide

**Arg:** `--common-log-key-hide`

**Description:** By default, we display the key name from the legend file over the actual command that was run, if the former exists. This flag instead shows the literal command. Commands without keys are unaffected.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --common-log-key-hide --config=examples/config.toml some-key</span>
<span style="color: #69ff94">[Success][echo hi && sleep 2] 2 seconds</span>
<span style="color: #d6acff">[Finished] 2 seconds</span></code>
</pre>

rather than the usual

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --config=examples/config.toml some-key</span>
<span style="color: #69ff94">[Success][some-key] 2 seconds</span>
<span style="color: #d6acff">[Finished] 2 seconds</span></code>
</pre>

Naturally, this does not affect commands that do not have a key (i.e. those not in a legend file). Also, if the commands are defined recursively, then the key name will be the _final_ key.

### Command Logging

Configuration for **command logs**, enabled by `console-log.command` and/or `file-logging`.

#### Buffer Length

**Arg:** `--command-log-buffer-length`

**Description:** Max text length held by the log buffer, used in conjunction with `--command-log-read-strategy block-line-buffer`. Defaults to 1,000 characters.

**Example:**

> [!NOTE]
> In this example, the log 'hi' is printed even though it is not newline-terminated, because the `--command-log-buffer-length 1` is exceeded (2 characters). On the other hand, the final log 'b' is not printed until the very end since it is within the buffer limit.

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --command-log-buffer-length 1 "printf hi && sleep 1 && printf b && sleep 2"</span>
<span style="color: #69ff94">[Command][printf hi && sleep 1 && printf b && sleep 1] hi</span>
<span style="color: #d6acff">[Timer] 1 second</span></code>
</pre>

#### Buffer Timeout

**Arg:** `--command-log-buffer-timeout`

**Description:** Max time the log buffer will hold a log before flushing it, used in conjunction with `--command-log-read-strategy block-line-buffer`. Defaults to 30 seconds.

**Example:**

> [!NOTE]
> In this example, the logs 'hi' and 'b' are printed even though they are not newline-terminated, because the `--command-log-buffer-timeout 1` is exceeded (1 second).

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --command-log-buffer-timeout 1 "printf hi && sleep 3 && printf b && sleep 1"</span>
<span style="color: #69ff94">[Command][printf hi && sleep 3 && printf b && sleep 1] hi</span>
<span style="color: #d6acff">[Timer] 1 second</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --command-log-buffer-timeout 1 "printf hi && sleep 3 && printf b && sleep 1"</span>
<span style="color: #69ff94">[Command][printf hi && sleep 3 && printf b && sleep 1] b</span>
<span style="color: #d6acff">[Timer] 3 seconds</span></code>
</pre>

#### Poll Interval

**Arg:** `--command-log-poll-interval NATURAL`

**Description:** Non-negative integer that determines how quickly we poll commands for logs, in microseconds. A value of 0 is interpreted as infinite i.e. limited only by the CPU. Defaults to 10,000.

> [!WARNING]
> Note that lower values will increase CPU usage. In particular, 0 will max out a CPU thread.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --command-log-poll-interval 100 --console-log-command "for i in {1..10}; do echo hi; sleep 1; done"</span>
<span style="color:">[Command][for i in {1..10}; do echo hi; sleep 1; done] hi</span>
<span style="color: #a3fefe">[Timer] 7 seconds</span></code>
</pre>

#### Read Size

**Arg:** `--command-log-read-size BYTES`

**Description:** The max number of bytes in a single read when streaming command logs. Logs larger than `--command-log-read-size` will be read in a subsequent read, hence broken across lines. The default is `16 kb`.

**Example:**

> [!NOTE]
> In this example we also use `--command-log-poll-interval 1_000_000` to slow down the reads, so that we can see `acbde` and `f` are indeed read separately. Ordinarily this would be too fast to see the difference.

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --command-log-read-size 5b --command-log-poll-interval 1_000_000 "echo abcdef && sleep 2" </span>
<span style="color:">[Command][echo abcdef && sleep 2] abcde</span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --command-log-read-size 5b --command-log-poll-interval 1_000_000 "echo abcdef && sleep 2" </span>
<span style="color:">[Command][echo abcdef && sleep 2] f</span>
<span style="color: #a3fefe">[Timer] 2 seconds</span></code>
</pre>


#### Read Strategy

**Arg:** `--command-log-read-strategy (block | block-line-buffer)`

**Description:** The `block` strategy reads `N` (`--command-log-read-size`) bytes at a time, whereas `block-line-buffer` also reads `N` bytes at a time, but buffers newlines, for potentially nicer formatted file logs. By default, we only use `block-line-buffer` when there is exactly one command. Otherwise we use `block`. This option explicitly sets the strategy.

> [!WARNING]
>
> The `block-line-buffer` strategy only makes sense when there is exactly _one_ command. Otherwise we could easily mix up logs from different commands, leading to nonsense output.

**Example:**

> [!NOTE]
> This is the previous example, but with `--command-log-read-strategy block-line-buffer` enabled. Notice the entire 'abcdef' is printed, since 'abcd' is read first, buffered, then 'f\n' is read, and the buffer flushed.

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --command-log-read-size 5b --command-log-poll-interval 1_000_000 --command-log-read-strategy block-line-buffer "echo abcdef && sleep 2" </span>
<span style="color:">[Command][echo abcdef && sleep 2] abcdef</span>
<span style="color: #a3fefe">[Timer] 2 seconds</span></code>
</pre>

### Console Logging

Config related to console logs.

#### Command Log

**Arg:** `--console-log-command`

**Description:** The default behavior is to swallow logs for the commands themselves. This flag gives each command a console region in which its logs will be printed. Only the latest log per region is shown at a given time.

> [!NOTE]
> When commands have complicated output, the logs can interfere with each other (indeed even overwrite themselves). We attempt to mitigate such situations: see [Strip Control](#strip-control).

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command "for i in {1..2}; do echo hi; sleep 1; done"</span>
<span style="color:">[Command][for i in {1..2}; do echo hi; sleep 1; done] hi</span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

vs.

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun "for i in {1..2}; do echo hi; sleep 1; done"</span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

> [!NOTE]
> Both the commands' `stdout` and `stderr` are treated the same, logged with the same formatting. This is because many shell programs perform redirection like `echo ... >&2` (i.e. redirect `stdout` to `stderr`). Not only does this mean we need to take both if we do not want to skip any output, but it also means it does not make sense to try to differentiate the two anymore, as that information has been lost.
>
> Practically speaking, this does not have much effect, just that if a command dies while `--console-log-command` is enabled, then the final `[Error] ...` output may not have the most relevant information. See [`--file-log`](#file-log) for details on investigating command failure.

#### Command Name Truncation

**Arg:** `--console-log-command-name-trunc NATURAL`

**Description:** Non-negative integer that limits the length of commands/key-names in the console logs. Defaults to no truncation.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command-name-trunc 10 "for i in {1..3}; do echo hi; sleep 1; done"</span>
<span style="color: #69ff94">[Success][for i i...] 3 seconds</span>
<span style="color: #d6acff">[Finished] 3 seconds</span></code>
</pre>

#### Line Truncation

**Arg:** `--console-log-line-trunc (NATURAL | detect)`

**Description:** Non-negative integer that limits the length of console logs. Can also be the string literal `detect`, to detect the terminal size automatically. Defaults to no truncation.

> [!NOTE]
>
> "log prefixes" (e.g. labels like `[Success]`, timestamps) are counted towards total length, but are never truncated.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --console-log-line-trunc 80 "echo 'some ridiculously long command i mean is this really necessary' && sleep 2"</span>
<span style="color:">[Command][echo 'some ridiculously long command i mean is this really necessary' && sleep 2] ...</span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

#### Strip Control

**Arg:** `--console-log-strip-control (all | smart | none)`

**Description:** Control characters can wreak layout havoc, thus we include this option. `all` strips all such chars. `none` does nothing i.e. all chars are left untouched. The default `smart` attempts to strip only the control chars that affect layout (e.g. cursor movements) and leaves others unaffected (e.g. colors). This has the potential to be the 'prettiest' as:

* Simple formatting is left intact.
* The layout should not be damaged.

Though it is possible to miss some chars. This option is experimental and subject to change.

**Example:**

Note: In the following examples, `\033[35m` and `\033[3D` are ansi escape codes. The former sets the text color to magenta, and the latter resets the cursor to the left by 3 places i.e. partially overwrites the previous characters. We also include the options `--console-log-command --console-log-command-name-trunc 10` (show command logs and truncate command name to 10 chars) to make the output easier to read.

`all` strips _all_ control characters: `\033` in this case. The means all special formatting / control will be omitted.
<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --console-log-command-name-trunc 10 --console-log-strip-control all "echo -e ' foo \033[35m hello \033[3D bye '; sleep 2"</span>
<span style="color:">[Command][echo -e...] foo  hello  bye</span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

`none` leaves all control characters in place. In this case, we will apply both the text coloring (`\033[35m`) and text overwriting (`\033[3D`).
<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --console-log-command-name-trunc 10 --console-log-strip-control none "echo -e ' foo \033[35m hello \033[3D bye '; sleep 2"</span>
<span style="color:">[Command][echo -e...] foo <span style="color: magenta"> hel bye</span></span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

`smart` removes the control chars but leaves the text coloring, so we will have the magenta text but not overwriting.
<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-command --console-log-command-name-trunc 10 --console-log-strip-control smart "echo -e ' foo \033[35m hello \033[3D bye '; sleep 2"</span>
<span style="color:">[Command][echo -e...] foo <span style="color: magenta"> hello  bye</span</span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

#### Timer Format

**Arg:** `--console-log-timer-format (digital_compact | digital_full | prose_compact | prose_full)`

**Description:** How to format the timer. Defaults to `prose_compact` e.g. `2 hours, 3 seconds`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-timer-format digital_compact "sleep 2"</span>
<span style="color: #a3fefe">[Timer] 01</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-timer-format digital_full "sleep 2"</span>
<span style="color: #a3fefe">[Timer] 00:00:00:01</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-timer-format prose_compact "sleep 2"</span>
<span style="color: #a3fefe">[Timer] 1 second</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --console-log-timer-format prose_full "sleep 2"</span>
<span style="color: #a3fefe">[Timer] 0 days, 0 hours, 0 minutes, 1 second</span></code>
</pre>

### File Logging

Config related to file logs.

#### File Log

**Arg:** `-f, --file-log (default | PATH)`

**Description**: If a path is supplied, all logs will additionally be written to the supplied file. Furthermore, command logs will be written to the file irrespective of [`--console-log-command`](#command-log). Console logging is unaffected. This can be useful for investigating command failures. If the string `default` is given, then we write to the XDG state directory e.g. `~/.local/state/shrun/shrun.log`.

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

#### File Command Name Truncation

**Arg:** `--file-log-command-name-trunc NATURAL`

**Description:** Like [`--console-log-command-name-trunc`](#command-name-truncation), but for `--file-logs`.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --file-log out.log --file-log-command-name-trunc 10 "for i in {1..3}; do echo hi; sleep 1; done"</span>
<span style="color: #69ff94">[Success][for i in {1..3}; do echo hi; sleep 1; done] 3 seconds</span>
<span style="color: #d6acff">[Finished] 3 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> cat out.log</span>
<span style="color:">[2024-04-23 01:05:21][Command][for i i...] Starting...</span>
<span style="color:">[2024-04-23 01:05:21][Command][for i i...] hi</span>
<span style="color:">[2024-04-23 01:05:22][Command][for i i...] hi</span>
<span style="color:">[2024-04-23 01:05:23][Command][for i i...] hi</span>
<span style="color:">[2024-04-23 01:05:24][Success][for i i...] 3 seconds</span>
<span style="color:">[2024-04-23 01:05:24][Finished] 3 seconds</span></code>
</pre>

#### File Delete On Success

**Arg:** `--file-log-delete-on-success`

**Description:** If `--file-log` is active, deletes the file on a successful exit. Does not delete the file if shrun exited via failure.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --file-log del-on-success.log --file-log-delete-on-success "sleep 2"</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #d6acff">[Finished] 2 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> cat del-on-success.log</span>
<span style="color:">cat: del-on-success.log: No such file or directory</span></code>
</pre>

vs.

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --file-log del-on-success.log --file-log-delete-on-success bad "sleep 2"</span>
<span style="color: #ff6e6e">[Error][bad] 0 seconds: /bin/sh: line 1: bad: command not found</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #d6acff">[Finished] 2 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> cat del-on-success.log</span>
<span style="color:">[2024-04-23 01:05:21][Command][bad] Starting...</span>
<span style="color:">[2024-04-23 01:05:21][Command][sleep 2] Starting...</span>
<span style="color:">[2024-04-23 01:05:21][Error][bad] 0 seconds: /bin/sh: line 1: bad: command not found</span>
<span style="color:">[2024-04-23 01:05:24][Success][sleep 2] 2 seconds</span>
<span style="color:">[2024-04-23 01:05:24][Finished] 2 seconds</span></code>
</pre>

#### File Line Truncation

**Arg:** `--file-log-line-trunc`

**Description:** Like [`--console-log-line-trunc`](#line-truncation), but for file logs.

**Example:**

<pre>
<code><span style="color: #ff79c6">$</span><span> shrun --file-log line-trunc.log --file-log-line-trunc 120 "echo 'some ridiculously long command i mean is this really necessary' && sleep 2"</span>
<span style="color: #69ff94">[Success][sleep 2] 2 seconds</span>
<span style="color: #d6acff">[Finished] 2 seconds</span></code>
</pre>

<pre>
<code><span style="color: #ff79c6">$</span><span> cat line-trunc.log</span>
<span style="color:">[2024-04-23 01:05:21][Command][echo 'some ridiculously long command i mean is this really necessary' && sleep 2] Star...</span>
<span style="color:">[2024-04-23 01:05:22][Command][echo 'some ridiculously long command i mean is this really necessary' && sleep 2] som...</span>
<span style="color:">[2024-04-23 01:05:24][Success][echo 'some ridiculously long command i mean is this really necessary' && sleep 2] 2 se...</span>
<span style="color:">[2024-04-23 01:05:24][Finished] 2 seconds</span></code>
</pre>

#### File Log Mode

**Arg:** `--file-log-mode (append | rename | write)`

**Description:** Mode in which to open the log file. Can be `write` (the default), `append`, or `rename`. The `rename` option will rename the requested log file if there is a collision e.g. `-f shrun.log` will become `shrun (1).log`.

#### File Log Size Mode

**Arg:** `--file-log-size-mode (warn BYTES | delete BYTES | nothing)`

**Description:** Sets a threshold for the file log size, upon which we either print a warning or delete the file, if it is exceeded. The `BYTES` should include the value and units e.g. `warn 10 mb`, `warn 5 gigabytes`, `delete 20.5B`. Defaults to warning at `50 mb`. Can be disabled with "nothing".

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

#### File Log Strip Control

**Arg:** `--file-log-strip-control (all | smart | none)`

**Description**: `--console-log-strip-control` for file logs created with `--file-log`. Defaults to all.

## Notifications

These options configure `shrun` to send off desktop notifications for certain actions i.e. a command finishes or shrun itself finishes.

### Notify Action

**Arg:** `--notify-action (final | command | all)`

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
