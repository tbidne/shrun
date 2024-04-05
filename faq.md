# FAQ

## What if a command needs sudo?

Commands _can_ receive `stdin`, so running e.g. `shrun "sudo some command"` will launch the sudo prompt. From there we can type the password and hit `enter` as usual.

However this is a bit clunky as the timer text will overwrite the `[sudo] password for ...` prompt, and multiple commands add more complication.

It is therefore easiest to run sudo first to elevate privileges, then execute `shrun` as normal e.g.

```
# Run sudo with some dummy command
$ sudo ls
...

# shrun can now execute sudo without requiring stdin
$ shrun ...
```

## What if my command relies on interactive shell e.g. loading ~/.bashrc?

Shrun executes shell commands non-interactively, which means we do not have access to anything defined in, say, `~/.bashrc`. This can be annoying if we want to run any of these functions/aliases.

```sh
# ~/.bashrc
foo () {
  ...
}

bar () {
  ...
}
```

```
$ shrun foo bar
[Error][bar] 0 seconds: /bin/sh: line 1: bar: command not found
[Error][foo] 0 seconds: /bin/sh: line 1: foo: command not found
[Finished] 0 seconds
```

Fortunately, the [`init`](configuration.md#init) option exists exactly for this purpose:

```
$ shrun --init ". ~/.bashrc" foo bar
```

This is equivalent to running:

```
$ shrun ". ~/.bashrc && foo" ". ~/.bashrc && bar"
```

> [!TIP]
>
> An extensive `~/.bashrc` may contain code that does not work well when loading non-interactively e.g. mine has the line `[[ $- == *i* ]] || return`, which `shrun` chokes on. Instead, you may want to create a file for your functions e.g. `~/.bash_functions.sh`, source _that_ in `~/.bashrc`, and then use it with `shrun` instead:
>
> ```
> shrun --init ". ~/.bash_functions.sh" ...
> ```

## Init vs. Legend

There are two ways to use command aliases with `shrun`. One is with the toml file's [`legend`](./configuration.md#legend) section:

```toml
legend = [
  { key = 'backend', val = 'javac ...' },
]
```

```
# runs javac as a shrun command
shrun -c config.toml backend
```

Another is with [`init`](configuration.md#init):

```
# e.g. define as bash alias/function instead in ~/.bashrc

backend () { javac ...; }
```

```
# runs the bash 'backend' function as a shrun command
shrun --init ". ~/.bashrc" backend
```

Why two methods?

1. The first reason is historical: `legend` preceded `init`, so for a time the former was the only way to use aliases with `shrun`.

2. Another reason for preferring `legend` is wanting a config specific to a certain project, rather than polluting something relatively global like `~/.bashrc`.

3. Finally, the primary reason is that the legend allows us to easily combine _multiple_ commands and keep `shrun`'s usual semantics (e.g. concurrency, independence):

    ```toml
    legend = [
      { key = 'backend', val = 'javac ...' },
      { key = 'ui', val = 'npm run build' },
      { key = 'all', val = ['backend', 'ui'] },
    ]
    ```

    ```
    # runs ui and backend concurrently
    shrun -c config.toml all
    ```

    On the other hand, the naive bash translation has different semantics:

    ```
    # e.g. define as bash alias/function instead in ~/.bashrc

    backend () { javac ...; }

    ui () { npm run build; }

    all () { backend; ui; }
    ```

    ```
    # runs the bash 'all' function as a shrun command, so backend and ui are
    # _not_ run concurrently / separately!
    shrun --init ". ~/.bashrc" all
    ```

    Of course you _can_ write concurrent bash code. But this problem was in fact `shruns`'s [motivating example](README.md#motivation).

Thus the conclusion is: If you have a single alias that you may want global (e.g. called with or without `shrun`), and you do not envision regularly running that alias simultaneously with other commands, by all means, throw it in e.g. `~/.bashrc` and use `init`.

If, instead, you don't want the alias in `~/.bashrc` or you regularly run it with some other commands, consider putting it in the toml's `legend`.

> [!TIP]
>
> You can also split the difference here. Put the individual commands `backend` and `ui` in `~/.bashrc`, load that with `init`, and put the aggregate `all` command in `legend`. This allows using `backend` and `ui` independent of `shrun`, while retaining `shrun`'s advantages with `all`:
>
> ```
> shrun --init ". ~/.bashrc" -c config.toml all
> ```