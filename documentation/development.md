# Development

## Architecture

### Effects

We use effectful-style programming, that is, writing type signatures like:

```haskell
logToFileQueue ::
  ( MonadSTM m,
    MonadTime m
  ) =>
  KeyHideSwitch ->
  FileLoggingEnv ->
  Log ->
  m ()
```

We use the `monad-effects` packages for this i.e. "MTL-style" effects over `ReaderT`.

### Concurrency

Threads are spawned for each of the following:

- Printing console logs.
- (Optional) printing file logs.
- Printing timer logs, checking `--timeout` if necessary.
- Each command.

Shrun and each command send their logs to `STM` containers, and the logger threads poll these queues, writing the results to their respective locations.

### Configuration

The configuration is done via CLI args and optional `toml` config file. In general, the data flow looks like:

```
CLI + TOML -> Merged -> Env
```

That is, we combine the `CLI` and `TOML` configuration (with `CLI` taking precedence) to produce a `Merged` configuration. We then turn that into the `Env` that is actually used to run our application. The `Merged` config is quite close to `Env`, though the latter will actually initialize resources as needed e.g. opening file handles.

In order to tame the number of types and impose some order, we employ the *Trees That Grow* approach. That is, data types are indexed by another type, which is then used as a parameter to a type family.

That is, we separate our configuration into **phases**, so that we can reuse the same types, while varying the fields as necessary:

```haskell
data ConfigPhase
  = ConfigPhaseArgs -- CLI args
  | ConfigPhaseToml -- TOML file
  | ConfigPhaseMerged -- CLI + TOML, filling in defaults
  | ConfigPhaseEnv -- Application environment
```

For example, the file logging configuration looks like:

```haskell
-- file logging data type
type FileLoggingP :: ConfigPhase -> Type
data FileLoggingP p = MkFileLoggingP
  { file :: FileLogFileF p,
    commandNameTrunc :: ConfigPhaseMaybeF p (Truncation TruncCommandName),
    deleteOnSuccess :: SwitchF p DeleteOnSuccessSwitch,
    lineTrunc :: LineTruncF p,
    stripControl :: ConfigPhaseF p FileLogStripControl
  }

-- main configuration data type
type CoreConfigP :: ConfigPhase -> Type
data CoreConfigP p = MkCoreConfigP
  { ...
    fileLogging :: ArgsOnlyDetF p (FileLoggingP p),
    ...
  }
```

Let's look at how this specializes across each data phase, particularly for the `file` field:

```haskell
-- ConfigPhaseArgs phase
data FileLogInitArgs = MkFileLogInitP
  { path :: WithDisabled FilePathDefault,
    mode :: WithDisabled FileMode,
    sizeMode :: WithDisabled FileSizeMode
  }

data FileLoggingArgs = MkFileLoggingP
  { file :: FileLogInitArgs,
    commandNameTrunc :: WithDisabled (Truncation TruncCommandName),
    deleteOnSuccess :: WithDisabled (),
    lineTrunc :: WithDisabled LineTruncation,
    stripControl :: WithDisabled FileLogStripControl
  }

data CoreConfigArgs = MkCoreConfigP
  { ...
    fileLogging :: FileLoggingArgs,
    ...
  }

```

The CLI `file` parameter is optional configuration for path, mode, and size-mode. These can also be disabled.

```haskell
-- ConfigPhaseToml phase
data FileLogInitToml = MkFileLogInitP
  { path :: FilePathDefault,
    mode :: Maybe FileMode,
    sizeMode :: Maybe FileSizeMode
  }

data FileLoggingToml = MkFileLoggingP
  { file :: FileLogInitToml,
    commandNameTrunc :: Maybe (Truncation TruncCommandName),
    deleteOnSuccess :: Maybe Bool,
    lineTrunc :: Maybe LineTruncation,
    stripControl :: Maybe (StripControl StripControlFileLog)
  }

data CoreConfigToml = MkCoreConfigP
  { ...
    fileLogging :: Maybe FileLoggingToml,
    ...
  }
```

The TOML `file` parameter is a mandatory path and optional mode and size-mode. Because the entire `fileLogging` itself is optional, making the path optional again would be redundant, hence it is required when `fileLogging` exists.

```haskell
-- ConfigPhaseMerged phase
data FileLogInitMerged = MkFileLogInitP
  { path :: FilePathDefault,
    mode :: FileMode,
    sizeMode :: FileSizeMode
  }

data FileLoggingMerged = MkFileLoggingP
  { file :: FileLogInitMerged,
    commandNameTrunc :: Maybe (Truncation TruncCommandName),
    deleteOnSuccess :: DeleteOnSuccessSwitch,
    lineTrunc :: Maybe (Truncation TruncLine),
    stripControl :: StripControl StripControlFileLog
  }

data CoreConfigMerged = MkCoreConfigP
  { ...
    fileLogging :: Maybe FileLoggingMerged,
    ...
  }
```

The Merged `file` parameter is a mandatory path, mode, size-mode. We now fill in missing defaults, so if `FileLoggingMerged` exists, so does every field.

```haskell
-- ConfigPhaseEnv phase
data FileLogOpened = MkFileLogOpened
  { handle :: ~Handle,
    queue :: ~(TBQueue FileLog)
  }

data FileLoggingEnv = MkFileLoggingP
  { file :: FileLogOpened,
    commandNameTrunc :: Maybe (Truncation TruncCommandName),
    deleteOnSuccess :: DeleteOnSuccessSwitch,
    lineTrunc :: Maybe (Truncation TruncLine),
    stripControl :: StripControl StripControlFileLog
  }

data CoreConfigEnv = MkCoreConfigP
  { ...
    fileLogging :: Maybe FileLoggingEnv,
    ...
  }
```

Finally, if `fileLogging` exists, `CoreConfigEnv`'s `file` parameter corresponds to the actually opened file handle, along with a queue for logging.

By doing this, we receive the following benefits:

- Sharing the same type means we do not have to worry about keeping multiple types in sync.
- The data evolutions process is cleanly organized. That is, the `CLI` and `TOML` are all self-contained. The `Merged` phase is responsible for combining the two and filling in required defaults. Finally, `Env` is what we use to run the application.

The type families keep us honest, so we can specify what types each phase should have, and then the compiler ensures that we meet this requirement.

### Misc

- We use the `HasX` pattern for the `ReaderT` environment.

- Optics (`optics-core`) are used heavily.

## Updating

### Shrun

When shrun itself is changed in a way that impacts the version, we need to update the following:

- `shrun.cabal.version`.
- `CHANGELOG.md`.
- `README.md`, `configuration.md` and/or `faq.md`, if necessary.
- `release_linux_static.shrun_vers`.
- `release_linux.shrun_vers`.
- `release_osx.shrun_vers`.

### General libraries

When we update shrun's dependencies, we may need to update the following:

- `cabal.project`:
  - `index-state`.
  - `source-repository-package` hashes.
  - `allow-newer`.
  - Regenerate freeze file / create new one.

- `shrun.cabal` dependencies (`cabal outdated`).

- `stack.yaml`:
  - `resolver`.
  - `extra-deps` hashes.
  - Regenerate lock file.

- Nix: `nix flake update`.

### GHC

This section concerns adding a new __major__ GHC version. We list 3 GHC versions at a given time, where the middle version is the "blessed" one. This means we will "drop" the oldest one, and add the new one.

We need to update the following references:

- `cabal.project`:
  - `index-state`.
  - `allow-newer`.
  - Create a freeze file for the new GHC.

- `shrun.cabal`:
  - `tested-with`.
  - `build-depends`, as needed (e.g. `base`).

- `stack.yaml`:
  - `resolver`.
  - Regenerate lock file.

- Nix:
  - `nix flake update`.
  - `flake.nix.ghc-version`.

- `bench/`:
  - Generate new benchmarks for local benchmarks.
  - Copy old newest baseline ci benchmarks to new GHC version. Replace with
    those generated by CI if necessary.

- `ci.yaml`:
  - `cabal.strategy.matrix.ghc.vers`: Drop the lowest version and add the new one.
  - `release_osx`: Update `ghc-version` and `cabal-version` if necessary.
  - `release_linux`: Update `ghc-version` and `cabal-version` if necessary.

- `Dockerfile`: Update `ghc`, `--project-file`, and `cabal` if necessary.

- `README.md.Building.Cabal`: Update the listed GHCs and blessed version.
