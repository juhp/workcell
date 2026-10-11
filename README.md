# workcell

Run installed system tools with a read-only filesystem, an isolated home,
and a writable project. Requires Linux and bubblewrap.

```sh
cabal build --offline
cabal run workcell -- --help
cabal run workcell -- -p . -- ghc --version
```

After installing with `cabal install`:

```sh
workcell                                  # current project, interactive Bash
workcell -p /path/to/project -- git status
workcell -H /path/to/sandbox-home          # persistent home, created if missing
workcell -v ~/.gitconfig -- git status    # read-only file at its original path
workcell -v ~/.cabal:rw -- cabal build --offline
workcell -v ~/.config/cabal -v ~/.cache/cabal:rw -v ~/.local/state/cabal:rw -- cabal build --offline
workcell -v /path/to/data:/tmp/data:ro -- ls /tmp/data
workcell -e LANG -e MY_VAR=hello -- env
workcell --dryrun -- ghc --version
workcell --share-processes -- ghc --version # fallback for restricted containers
workcell --share-filesystem -- ls /var/log # expose enclosing filesystem read-only
workcell -v /path/to/data:/opt/data:ro -- ls /opt/data
```

`-p/--project` defaults to the current directory. The project is mounted writable
at its original absolute path (resolved through symlinks); commands start there.
Edits and deletions affect the original project immediately.

Home is mounted at the invoking user's original home path (resolved through
symlinks). It is empty and temporary by default. `-H/--home DIR` uses a persistent
writable directory instead, creating it if missing. The host's original home
contents are hidden except for the project and explicitly selected volumes.

Repeatable `-v/--volume SOURCE[:DEST][:ro|rw]` mounts an existing file or directory.
The destination defaults to the source's absolute path, and access defaults to
read-only. Relative paths resolve against the invoking directory. Paths containing
colons and Podman-specific options are unsupported. New destinations and parent
directories are created inside the sandbox, including beneath `/opt`, `/srv`, or
other unexposed paths. Beneath read-only system mounts such as `/usr` and `/etc`,
destinations must already exist. Volume destinations
must not overlap the project or cover the isolated home; volumes beneath home are
allowed. Duplicate destinations and file/directory type mismatches are rejected.

The environment is cleared except for a default `PATH`, selected `HOME`, `TERM`,
`WORKCELL_ACTIVE=1`, and inherited locale settings (`LANG`, `LANGUAGE`, and `LC_*`). This lets programs
such as tmux detect UTF-8 before starting a shell. Unset locale variables remain
unset; workcell does not choose a locale on your behalf.
Repeatable `-e/--env KEY` passes through an existing variable (an unset variable is
an error); `KEY=VALUE` sets it, including empty values. Later entries override
earlier entries and defaults. The default `PATH` searches `$HOME/.local/bin`,
then `$HOME/bin`, followed by the system directories. Home-installed tools need
a persistent home or an explicit mount (for example, `-v ~/.local/bin` or
`-v ~/bin`); the default home is empty. Tools in other locations need a supplied
`PATH`.

Use `WORKCELL_ACTIVE` to detect workcell in your shell configuration. For example,
add this after setting `PS1` in the `.bashrc` used by your workcell home:

```bash
if [[ ${WORKCELL_ACTIVE:-} == 1 ]]; then
    PS1="[workcell] $PS1"
fi
```

With the default empty home, mount your Bash configuration explicitly (for
example, `workcell -v ~/.bashrc`) or use a persistent home with `--home`.

Options precede the command. Use `--` to separate command arguments; those
arguments are passed directly without shell expansion. Without a command,
workcell starts `/bin/bash`. `--dryrun` prints a shell-quoted
bubblewrap invocation without launching it or creating a home directory.
Dry-run output includes explicitly supplied environment values.

The default filesystem starts empty and exposes `/usr` and `/etc` read-only,
plus the system's executable/library aliases (`/bin`, `/sbin`, `/lib`, `/lib64`).
Enclosing `/var`, `/opt`, `/srv`, and `/sys` contents are hidden. `/var/home`
remains an alias to `/home` on systems that use it. The root is read-only after
mount setup, with temporary `/home`, `/root`, `/tmp`, `/run`, and `/var/tmp`
directories and a minimal `/dev`. Temporary files outside writable mounts
disappear on exit. Hardware and SELinux status paths beneath `/sys` require an
explicit volume if needed.

`--share-filesystem` restores read-only visibility of the enclosing filesystem,
while still isolating home and the temporary directories and keeping private
processes by default. This compatibility option is independent of
`--share-processes`. New destinations in this mode must be under isolated home
or temporary directories; other destinations must already exist. Neither option
is enabled automatically when sandbox setup fails.

By default, workcell creates a private PID namespace
and mounts a fresh `/proc`, so enclosing processes are not visible there. It
checks that this is supported before creating a persistent home or launching the
command, and fails with a diagnostic rather than silently weakening isolation.
Volumes, home, and project mounts must not replace the private `/proc`.

Some restricted containers, including the encapsule environment used during
development, prevent mounting a fresh `/proc`. `--share-processes` explicitly
uses the enclosing PID namespace and existing read-only `/proc` instead. This
exposes enclosing process information and potentially filesystem handles.
Network access remains shared in both modes.
Individual targets of `/etc/resolv.conf`, `/etc/hosts`, `/etc/host.conf`, and
`/etc/localtime` are preserved read-only when they live outside the exposed
system directories, as they commonly do in Toolbx. Their enclosing directories
remain hidden.

For nested bubblewrap in encapsule, a capability can supply Podman security
options. One candidate to test in a new encapsule container is:

```toml
[capabilities.nested-bwrap]
security_opts = ["unmask=/proc/*"]
```

Use `encapsule run IMAGE --cap nested-bwrap -- bwrap --ro-bind / / --unshare-pid --proc /proc --dev /dev /bin/ps -ef`
to check whether it helps. This relaxes outer `/proc` masking; it is not a
confirmed fix, since syscall filtering or other policies can also prevent
the mount. See [encapsule capabilities](https://github.com/juhp/encapsule#capabilities)
and [Podman security options](https://docs.podman.io/en/stable/markdown/podman-run.1.html#security-opt-option).

This is a filesystem isolation experiment, not a sandbox for untrusted code.
System configuration under `/etc` remains readable, and network services remain
reachable through shared networking. Explicit mounts and the compatibility
options can weaken isolation. Named capabilities,
network isolation, and syscall filtering are deferred.

Run the Hspec integration suite with Cabal:

```sh
cabal test --offline --test-show-details=direct
```

The checks exercise both process modes when private `/proc` is supported. In a
restricted environment they verify clear default failure and run the other live
checks with the explicit `--share-processes` fallback.
The suite runs the executable built by Cabal; set `WORKCELL` to an absolute path
to test another binary. Each example uses a separate temporary project and
cleans it up afterward. Bubblewrap is required; GHC and `/var/home` checks are
marked pending when unavailable.

The original shell prototype is also available:

```sh
./bwrap.sh                       # current project, interactive Bash
./bwrap.sh /path/to/project      # another project
./bwrap.sh . ghc --version       # run a command
```

The shell prototype uses the fixed temporary home `/home/workcell` and has no
extra volume or environment options.
