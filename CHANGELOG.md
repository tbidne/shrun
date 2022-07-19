# Revision history for shrun

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Haskell Package Versioning Policy](https://pvp.haskell.org/).

Note that we PVP is applied to the _application_, not _library_. That is, the
Major/Minor/Patch definitions apply to the application's interface / usage
(e.g. cli args, config file), not the library.

## [Unreleased]
### Changed
* Renamed executable from `shell-run` to `shrun`.
* Added toml configuration. This subsumes the old `--legend` file, as that is
  part of the new toml file.
* `--key-show` is now the default behavior. The flag has thus been renamed
  `--key-hide`, and is off by default.
* `--file-logging` no longer interprets `"d"` and `"default"`
  as the default paths. Instead it expects an empty string e.g.
  `--file-logging=` or `-f ''`.

### Fixed
* Improve `--strip-control`. `all` should no longer leave ansi remnants, and
  `--smart` is much more aggressive about removing everything but harmless
  styling.
* Improve benchmarking to more accurately describe memory usage.

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

[Unreleased]: https://github.com/tbidne/shrun/compare/0.1...HEAD
[0.4]: https://github.com/tbidne/shrun/compare/0.3..0.4
[0.3]: https://github.com/tbidne/shrun/compare/0.2.0.1..0.3
[0.2.0.1]: https://github.com/tbidne/shrun/compare/0.2..0.2.0.1
[0.2]: https://github.com/tbidne/shrun/compare/0.1..0.2
[0.1]: https://github.com/tbidne/shrun/releases/tag/