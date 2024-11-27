# Revision history for shrun

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

Note that PVP is applied to the _application_, **not** the library. That is,
the major/minor/patch definitions apply to the application's interface / usage
(e.g. cli args, config file), not the library.

For those unfamiliar with PVP, it is essentially
[SemVer](https://semver.org/spec/v2.0.0.html), except the PVP major version is
the first _two_ components (e.g. `0.9` in `0.9.1`), and PVP has no exception
for versions < 1.

## [Unreleased]
### Added
* Add `--file-log-mode rename`, that renames the requested log file if it
  already exists. E.g. `-f shrun.log` becomes `shrun (1).log`.

## [0.9.1] -- 2024-07-27
### Changed
* Updated blessed GHC to 9.8.2.

### Added
* Add `--command-log-read-strategy (command-log.read-strategy)` option that
  allows for line buffering.
* Add `--command-log-buffer-length` and `--command-log-buffer-timeout` options
  for use with `--command-log-read-strategy block-line-buffer`.
* Added GHC 9.10 support.

### Fixed
* Line truncation `detect` option now subtracts one, so terminals do not add
  an extra newline.
* More `HasCallStack` annotations for better callstacks in case something goes
  wrong.
* Increase default `--command-log-read-size` to `16 kb` to better prevent
  command log splitting.
* Improved formatting. Leading/trailing whitespace is no longer stripped from
  file logs, so original formatting is preserved.
* Simplify reading final error message.
* Hide `--version` from primary usage section.

## [0.9] -- 2024-05-10
### Changed
* `--notify-action command` (`notify.action = "command"`) no longer implies
  `--notify-action final`. There is a new option `--notify-action all` for
  that.
* `--file-log-size-mode` now defaults to `warn 50 mb` (i.e. warn when the log
  file exceeds 50 mb). There is a new option `nothing` to disable this.
* Extensive CLI/toml option changes/renaming:
  * `--key-hide`:
    * CLI: `--common-log-key-hide`.
    * Toml: `common-log.key-hide`.
    * `-k` removed.
  * `--timer-format`:
    * CLI: `--console-log-timer-format`.
    * Toml: `console-log.timer-format`.
  * `--poll-interval`:
    * CLI: `--command-log-poll-interval`.
    * Toml: `command-log.poll-interval`.
    * `-p` removed.
  * `--cmd-log`:
    * CLI: `--console-log-command`.
    * Toml: `console-log.command`.
    * `-l` removed.
  * `--cmd-name-trunc`:
    * CLI: `--console-log-command-name-trunc`.
    * Toml: `console-log.command-name-trunc`.
    * `-x` removed.
    * Applies to all console logs, not just command logs.
  * `--cmd-log-line-trunc`:
    * CLI: `--console-log-line-trunc`.
    * `-y` removed.
    * Toml: `console-log.line-trunc`.
  * `--cmd-log-strip-control`:
    * CLI: `--console-log-strip-control`.
    * Toml: `console-log.strip-control`.
    * `-s` removed.

### Added
* Added stack support back.
* New option `--command-log-read-size` (toml: `command-log.read-size`) that controls
  the size of logs we read from command with `--console-cmd-log` and
  `--file-log`.
* New option `--file-log-command-name-trunc` (toml: `file-log.command-name-trunc`)
  for command name truncation in the file logs.
* New option `--file-log-delete-on-success`
  (toml: `file-log.delete-on-success`) that deletes the log file upon a
  successful exit.
* New option `--file-log-line-trunc`
  (toml: `file-log.line-trunc`) for line truncation in the file logs.
* Options taking `NATURAL` numbers now allow underscore separators.

### Fixed
* Improved `strip-control` options `all` and `smart` to replace newlines with
  a single whitespace, rather than stripping them.
* Notification failures are no longer swallowed; errors do not kill shrun,
  but they are logged and cause the program to exit with a failure code.
* Fixed bug where `--notify-system notify-send` failed when given legend
  commands containing quotes and `--log-key-hide` was active.

## [0.8.1] -- 2023-12-08
### Changed
* Default log renamed from `XDG_STATE/shrun/log` to
  `XDG_STATE/shrun/shrun.log`.

### Added
* Added feature for sending notifications upon command/shrun completion.
  The new CLI options (`toml`) are:
  * `--notify-action (notify.action)`
  * `--notify-system (notify.system)`
  * `--notify-timeout (notify.timeout)`
* Added new (`--timer-format` / `timer-format`) feature for formatting the
  timer. Options are:
  * `digital_compact`
  * `digital_full`
  * `prose_compact`
  * `prose_full`
* Add `no-x` options to allow selectively disabling options (e.g. disable a
  single field set by the toml file).

### Fixed
* Fixed bug where command logs with newlines in them were rendered in the same
  log. These are now split across multiple logs.
* Help page improved.

## [0.8] -- 2023-03-22
### Removed
* `stack` support removed.

### Changed
* If any commands fail, shrun now exits with an error code. Previously, shrun
  would exit successfully in this scenario, returning an exit code only when
  shrun _itself_ died.
* Similarly, error code is now set if any commands time out.
* File logging now respects `--key-hide` i.e. default behavior matches
  `--cmd-log` and prints the key name over the literal command.
* We now default logging to the XDG State directory rather than XDG config
  i.e. `~/.local/state/shrun/log`.

### Added
* `--init` (toml: `init`) option that allows one to specify logic that should
  be run before each command. This can be useful when we want to load
  functions/aliases e.g. `shrun --init ". ~/.bashrc" foo`.
* `osx` support.

### Fixed
* File logs are now streamed, do not buffer.
* Previously, if `--cmd-log` or `--file-log` were specified, then a CPU thread
  would be maxed out polling commands for logs. We have now set a delay,
  defaulting to 10,000 microseconds, to keep the CPU reasonable. This value
  can be configured through the new `--poll-interval` option.

## [0.7] -- 2022-12-19
### Removed
* `log-disable` option removed from CLI and TOML.

### Changed
* Logs
  * Slightly more compact now (spaces between labels removed)
  * Success/Finished message now have specific labels
  * Timer logs not sent to file

### Fixed
* Fixed bug where legend key with multiple values i.e. `k = [v1, v2, ...]`
  would consider each `vi` to have key name `k`. This meant output could be
  ambiguous with `key-hide = false` (the default) because multiple values
  would have the same key name. Now we only consider a value to have a key
  name when it is unique i.e. `k = v`.
* File logging thread no longer polls when file logging is disabled.
* File logging no longer crashes if the log file does not already exist.
* Final log message now consistently show up in the log file.
* Fixed bug where ansi control chars could "bleed" over into other logs
* Control chars stripped from command names every time.
* Fixed bug where the toml file's `cmd-log.line-trunc` and
  `strip-control.cmd-log` were not overridden by the CLI unless the CLI also
  specified `--cmd-log`.


## [0.6] -- 2022-12-07
### Changed
* CLI
  * `cmd-line-trunc` renamed to `cmd-log-line-trunc`.
  * `strip-control` renamed to `cmd-log-strip-control`.
  * `disable-log` renamed to `log-disable`.
* TOML
  * Commands logging is now part of the table `cmd-log`.
    * `cmd-line-trunc` renamed to `cmd-log.line-trunc`.
    * `strip-control` renamed to `cmd-log.strip-control`.
  * File logging is now part of the table `file-log`.
    * `file-log` renamed to `file-log.path`.
    * `file-log-mode` renamed to `file-log.mode`.
    * `file-log-strip-control` renamed to `file-log.strip-control`.
* Timestamps in logs are now simpler: No timezone or fractions of a second.

### Fixed
* Fix redundant COMMANDS... on help page.

## [0.5] -- 2022-08-08
### Changed
* Renamed executable from `shell-run` to `shrun`.
* Added toml configuration. This subsumes the old `--legend` file, as that is
  part of the new toml file.
* `--key-show` is now the default behavior. The flag has thus been renamed
  `--key-hide`, and is off by default.
* `--file-log` no longer interprets `d` as the default path. Only the string
  `default` is expected.
* Renamed toml's `--file-logging` to `--file-log` for consistency with CLI.

### Added
* `--file-log-strip-control` for adding `--strip-control` functionality
  to file logs.
* `--file-log-mode` option for choosing between `append` and `write` file
  modes.
* `--file-log-size-mode` option for warning or automatically deleting the
  log file if it crosses a user-specified threshold.

### Fixed
* Improve `--strip-control`. `all` should no longer leave ansi remnants, and
  `smart` is much more aggressive about removing everything but harmless
  styling.
* Improve benchmarking to more accurately describe memory usage.
* File logging handled more intelligently: no longer opening/closing on each
  write.

## [0.4] -- 2022-06-21
### Changed
* File logging in local time, not UTC.

### Fixed
* Improved integration tests.

## [0.3] -- 2022-05-28
### Added
* Add new `--strip-control` argument for handling control chars in logs.

## [0.2.0.1] -- 2022-05-24
### Changed
* Update CI jobs

### Fixed
* Performance improvements
* Documentation

## [0.2] -- 2022-05-11
### Changed
* Update default legend: legend.txt -> shrun.legend
* Update default logs: logs.txt -> shrun.log

## [0.1] -- 2022-04-03

* First version. Released on an unsuspecting world.

[0.9.1]: https://github.com/tbidne/shrun/compare/0.9...0.9.1
[0.9]: https://github.com/tbidne/shrun/compare/0.8.1...0.9
[0.8.1]: https://github.com/tbidne/shrun/compare/0.8...0.8.1
[0.8]: https://github.com/tbidne/shrun/compare/0.7...0.8
[0.7]: https://github.com/tbidne/shrun/compare/0.6...0.7
[0.6]: https://github.com/tbidne/shrun/compare/0.5...0.6
[0.5]: https://github.com/tbidne/shrun/compare/0.4..0.5
[0.4]: https://github.com/tbidne/shrun/compare/0.3..0.4
[0.3]: https://github.com/tbidne/shrun/compare/0.2.0.1..0.3
[0.2.0.1]: https://github.com/tbidne/shrun/compare/0.2..0.2.0.1
[0.2]: https://github.com/tbidne/shrun/compare/0.1..0.2
[0.1]: https://github.com/tbidne/shrun/releases/tag/0.1
