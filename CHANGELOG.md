# Revision history for shell-run

## 0.5

* Added toml configuration. This subsumes the old `--legend` file, as that is
  part of the toml file.
* Improve `--strip-control`. `all` should no longer leave ansi remnants, and
  `--smart` is much more aggressive about removing everything but harmless
  styling.
* `--key-show` is now the default behavior. The flag has thus been renamed
  `--key-hide`, and is off by default.
* `--file-logging` no longer interprets `"d"` and `"default"`
  as the default paths. Instead it expects an empty string e.g.
  `--file-logging=` or `-f ''`.
* Improve benchmarking to more accurately describe memory usage.

## 0.4 -- 2022-06-21

* File logging in local time, not UTC.
* Internal improvements (type safety, linting/ci ease).
* Improve integration test.

## 0.3 -- 2022-05-28

* Adds new `--strip-control` argument for handling control chars in logs.

## 0.2.0.1 -- 2022-05-24

* Update documentation
* Update CI jobs
* Add INLINEABLE

## 0.2 -- 2022-05-11

* Update default legend: legend.txt -> shell-run.legend
* Update default logs: logs.txt -> shell-run.log

## 0.1 -- 2022-04-03

* First version. Released on an unsuspecting world.
