WIP (working title)

# Workcell

Workcell is a Linux CLI tool using Bubblewrap to setup Linux namespaces
to run programs with a read-only sysroot, an isolated homedir and restricted
to a writable project/work dir (and runtime). By default it attempts private
networking using pasta, falling back to host networking if it fails.

## Usage

`$ workcell --version`

```
0.1.1
```

`$ workcell --help`

```
workcell

Usage: workcell [--version] COMMAND

  Run tools inside bwrap with an isolated home and writable project

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  list                     List active workcells in this PID namespace
  enter                    Enter an active workcell without sudo
  run                      Run a workcell bwrap session
```

### `run` command
The main way to use workcell is to start a bwrap session.

`$ workcell run --help`

```
Usage: workcell run [-p|--project DIR[:(ro|rw|O)]] [-H|--home DIR]
                    [-v|--volume SOURCE[:DEST][:(ro|rw|O)]]
                    [-e|--env KEY[=VALUE]] [--wayland] [--x11] [--dbus]
                    [--dryrun] [--share-processes] [--share-filesystem]
                    [--network-host | --network-none | --network-private]
                    [[--] COMMAND ARG...]

  Run a workcell bwrap session

Available options:
  -p,--project DIR[:(ro|rw|O)]
                           Project access (default: rw; :O for temporary
                           overlay; directory defaults to cwd, excluding HOME
                           and ancestors)
  -H,--home DIR            Persistent writable home (created if missing)
  -v,--volume SOURCE[:DEST][:(ro|rw|O)]
                           Extra mount (:O for temporary directory overlay;
                           default: same path, read-only)
  -e,--env KEY[=VALUE]     Set or pass through an environment variable
  --wayland                Expose the Wayland display socket for graphical
                           applications
  --x11                    Expose the local X11 display socket and
                           authentication file
  --dbus                   Expose the desktop D-Bus session socket (unfiltered)
  --dryrun                 Print the bubblewrap command without changing files
  --share-processes        Share enclosing processes and /proc instead of
                           isolating them
  --share-filesystem       Expose the enclosing filesystem read-only, retaining
                           isolated home
  --network-host           Use host networking
  --network-none           Disable networking except private loopback
  --network-private        Use a private pasta network (attempted by default)
  -h,--help                Show this help text
```

Default networking tries to set up private networking
but falls back to host networking if its setup fails.
Workcell will print a warning before launching the command.
`--network-private` requires successful private setup.

User and group names are resolved before isolation.
For SSSD and other directory accounts, workcell adds missing entries
for the invoking user and groups to read-only sandbox copies of
`/etc/passwd` and `/etc/group`.

### `list` and `enter` commands

Use `workcell list` to list running workcell sessions.

Use `workcell enter` to join a session:

`$ workcell enter --help`

```
Usage: workcell enter [--dryrun] [SESSION] [--root] [[--] COMMAND ARG...]

  Enter an active workcell without sudo

Available options:
  --dryrun                 Print the entry command without entering
  --root                   Enter as root inside the workcell
  -h,--help                Show this help text
```
NB Ending the `run` session first currently kills any `enter` one running.
This could be improved later. Recommend running tmux inside workcell instead.

## Examples

```shellsession
$ workcell run   # open a workcell shell on the cwd
$ workcell run --project /path/to/project -- git status
$ workcell run -p /path/to/project:ro -- git diff   # read-only project
$ workcell run -p /path/to/project:O -- make   # discard project changes on exit
$ workcell run --home /path/to/sandbox-home   # persistent home, created if missing
$ workcell run -v ~/.gitconfig -- git status    # mount file ro
$ workcell run -v ~/.cargo:rw -- cargo build
$ workcell run -v ~/.config/cabal -v ~/.cache/cabal:rw -v ~/.local/state/cabal:rw -- cabal build --offline
$ workcell run -v /path/to/data:/tmp/data:ro -- ls /tmp/data
$ workcell run -v /path/to/data:/tmp/data:O   # temporary writable view of data
$ workcell run -e LANG -e MY_VAR=hello -- env
$ workcell run --dryrun -- gcc --version
$ workcell run --share-processes   # fallback for restricted containers
$ workcell run --share-filesystem -- ls /var/log   # expose enclosing filesystem read-only
$ workcell run --network-none   # offline, separate loopback network
$ workcell run --network-host -- curl localhost:8000   # share enclosing network
$ workcell run --wayland gtk4-demo
$ workcell run --x11 xterm
```

`:O` requires a directory and bubblewrap with overlay support. Changes to that
mount are discarded when the workcell exits; the source directory stays unchanged.

### Entering an active workcell

From another terminal in the same enclosing environment:

```sh
workcell list
workcell enter                       # use the only active session; starts Bash
workcell enter s123456               # use an ID from workcell list; starts Bash
workcell enter s123456 -- git status
workcell enter s123456 --root        # root inside the workcell
workcell enter s123456 --dryrun
```

The session ID is optional. With one active session, `enter` uses it automatically.
With multiple sessions and terminal input, it shows a numbered menu and prompts
for a choice; press Enter to cancel. Without terminal input, it lists the sessions
and returns an error asking for an ID. With no active sessions, it reports an error.
`--root` and `--dryrun` also work without an ID. Supply an explicit ID when passing
a command.

## Requirements
Linux

- bwrap (bubblewrap)
- pasta (passt)
- nsenter (util-linux)

## Installation

workcell is packaged in Fedora Copr:

<https://copr.fedorainfracloud.org/coprs/petersen/workcell/>

## Building

Requires ghc and cabal-install (or stack).

On Fedora: install system deps: `cabal-rpm builddep`.

Then run `cabal install` (or `stack install`)

There is also an extensive testsuite and benchmarking.

## Contributing and development

Workcell is distributed under LGPLv2.1+

<https://github.com/juhp/workcell>

See also the generated [Notes](NOTES.md) for various more details.
