# Development

## Architecture

### Effects

We use effectful-style programming, that is, writing type signatures like:

```haskell
logToFileQueue ::
  ( MonadAtomic m,
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
- Draining stdin.

Shrun and each command send their logs to `STM` containers, and the logger threads poll these queues, writing the results to their respective locations.

### Configuration

The configuration is done via CLI args and optional `toml` config file. In general, the data flow looks like:

```
CLI + TOML -> Merged -> Env
```

We combine the `CLI` and `TOML` configuration (with `CLI` taking precedence) to produce a `Merged` configuration. We then turn that into the `Env` that is actually used to run our application. The `Merged` config is quite close to `Env`, though the latter will actually initialize resources as needed e.g. opening file handles.

In order to tame the number of types and impose some order, we employ the *Trees That Grow* approach. That is, data types are indexed by another type, which is then used as a parameter to a type family.

In particular, we separate our configuration into **phases**, so that we can reuse the same types, while varying the fields as necessary:

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

The CLI `file` parameter is `WithDisabled` for path, mode, and size-mode. This is like `Maybe`, hence optional, with an additional option for disabling the field entirely.

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

The TOML `file` parameter, on the other hand, is a mandatory path and optional mode and size-mode. Because the entire `FileLoggingToml` itself is optional, if it is specified in the TOML, we require the path since there is no reason to specify configuration if the path itself is not given.

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

The Merged `file` parameter is a mandatory path, mode, size-mode. We now fill in missing values with defaults, so if `FileLoggingMerged` exists, so does every field.

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

Finally, if `FileLoggingEnv` exists, `CoreConfigEnv`'s `file` parameter corresponds to the actually opened file handle, along with a queue for logging. To summarize, we have evolved our file configuration thusly:

```
(optional) parameters -> combined configuration -> open file
```

By doing this, we receive the following benefits:

- Sharing the same type means we do not have to worry about keeping multiple types in sync.
- The data evolutions process is cleanly organized. That is, the `CLI` and `TOML` are all self-contained. The `Merged` phase is responsible for combining the two and filling in required defaults. Finally, `Env` is what we use to run the application.

Furthermore, it is easy to see how the data evolves: just look at the type family for a given field. There is no need to dig through large functions just to see what a particular field is or where a default value might be given.

### Misc

- We use the `HasX` pattern for the `ReaderT` environment.

- Optics (`optics-core`) are used heavily.

## Updating

### Shrun

When shrun itself is changed in a way that impacts the version, we need to update the following:

- `shrun.cabal.version`.
- `CHANGELOG.md`.
- `README.md`, `configuration.md` and/or `faq.md`, if necessary.
- `ci.yaml.env.SHRUN_VERS`

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

This section concerns adding a new __major__ GHC version. At the very least
we need to update:

- `cabal.project`:
  - `index-state` as needed.
  - `allow-newer` as needed.

- `shrun.cabal`:
  - `tested-with`.
  - `build-depends`, as needed (e.g. `base`).

- `bench/`:
  - Generate new benchmarks for local benchmarks.
  - Copy old newest baseline ci benchmarks to new GHC version. Replace with
    those generated by CI if necessary.

- `ci.yaml`:
  - `cabal.strategy.matrix.ghc.vers`: Add the new one.

- `README.md.Building.Cabal`: Update the listed GHCs.

If we also want to ugrade the "blessed" (i.e. release) compiler, then we also
need to update:

- Create a freeze file for the new GHC.

- `stack.yaml`:
  - `resolver`.
  - Regenerate lock file.

- Nix:
  - `nix flake update`.
  - `flake.nix.ghc-version`.

- `ci.yaml`:
  - `env.CABAL_VERS` if necessary.
  - `env.GHC_VERS`
  - `env.CABAL_PROJ`
  - `cabal.strategy.matrix.ghc.proj-file`: Swap the new freeze file.
