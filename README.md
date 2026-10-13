WIP

# workcell notes

Run installed system tools with a read-only filesystem, an isolated home,
and a writable project. Requires Linux and bubblewrap, plus pasta (from the
`passt` package) for the default private networking mode.

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
workcell --network none -- ghc --version   # offline, separate loopback network
workcell --share-network -- curl localhost:8000 # share enclosing network
workcell -v /path/to/data:/opt/data:ro -- ls /opt/data
```

## Entering an active workcell

From another terminal in the same enclosing environment:

```sh
workcell --list
workcell --enter s123456               # use an ID from --list; starts Bash
workcell --enter s123456 -- git status
workcell --enter s123456 --dryrun
```

Entry requires `nsenter` (from util-linux) and runs without sudo. It starts an
additional command using the session's filesystem root, working directory,
initial command environment, and isolated process/network namespaces. Shared
process or network namespaces are left unchanged. Session operations cannot be
combined with launch options such as `--home`, `--volume`, or `--network`.

Records are stored beneath `$XDG_RUNTIME_DIR/workcell/sessions/<pid-namespace-id>/`.
When no absolute runtime directory is configured, workcell uses
`$XDG_STATE_HOME/workcell/sessions/`, or `~/.local/state/workcell/sessions/` if
that variable is also unset or relative, with the same namespace subdirectory.
Session directories are mode 0700 and records are mode 0600; records contain
launch metadata but no environment values. Launch supervision also requires a
writable enclosing temporary directory.

Listing and entry validate the PID, process start time, boot ID, and namespace
identity. Normal exit and termination remove the record; an uncatchable stop
may leave a stale record, which is ignored. Only sessions started by this
version are registered. Discovery is scoped to the caller's PID namespace:
sharing a runtime directory between toolboxes does not by itself allow entry
from another PID or user namespace. Namespace permissions still apply.

Entry can appear as UID 0 in the namespace owning the filesystem. This maps to
your enclosing user and does not grant host-root identity. It joins namespaces;
it does not automatically inherit seccomp filters or resource limits from the
original command. For manual host-root entry, use a host-visible command PID:

```sh
sudo nsenter --target PID --mount --net --pid --root --wd -- /bin/bash
```

`-p/--project` defaults to the current directory. The project is mounted writable
at its original absolute path (resolved through symlinks); commands start there.
Edits and deletions affect the original project immediately.
When the current directory is your home or one of its ancestors, the implicit
project is omitted and commands start in the isolated home instead. Explicit
`--project` paths covering your home are rejected, since mounting them would
expose the original home over the isolated home.

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

`--wayland` exposes the individual Wayland display socket and sets
`XDG_RUNTIME_DIR`, `WAYLAND_DISPLAY`, and `GDK_BACKEND=wayland` for graphical
applications. It requires an absolute `XDG_RUNTIME_DIR` and an existing socket.
If `WAYLAND_DISPLAY` is unset, it uses `wayland-0`; absolute display paths are
also supported. Explicit `-e` settings override these defaults.

```sh
workcell --wayland -- gtk4-demo
```

Audio, GPU devices, and the desktop D-Bus session are not exposed yet
by this option.

`--x11` supports local X11 and Xwayland displays such as `:0` or `:0.0`.
It exposes only the selected `/tmp/.X11-unix/XN` socket and mounts the authority
file from `$XAUTHORITY` (or `~/.Xauthority` when unset) read-only at
`/run/workcell-xauthority`. It sets `DISPLAY`, `XAUTHORITY`, and
`GDK_BACKEND=x11`; explicit `-e` settings override these defaults.
The socket and authority file must exist. TCP displays, including SSH-forwarded
displays such as `localhost:10.0`, are unsupported by this option.
`--x11` and `--wayland` cannot be combined.

```sh
workcell --x11 -- xterm
```

X11 access permits broad interaction with other X clients, including input and
clipboard access. Audio, GPU devices, and desktop D-Bus remain separate.

Use `WORKCELL_ACTIVE` to detect workcell in your shell configuration.
For example, add this after setting `PS1` in the `.bashrc` used by
your workcell home:

```bash
if [[ ${WORKCELL_ACTIVE:-} == 1 ]]; then
    PS1="[workcell] $PS1"
fi
```

With the default empty home, mount your Bash configuration explicitly (for
example, `workcell -v ~/.bashrc`) or use a persistent home with `--home`.

Options precede the command. Use `--` to separate command arguments; those
arguments are passed directly without shell expansion. Without a command,
workcell starts `/bin/bash`. `--dryrun` prints a shell-quoted launch without
launching it or creating a home directory. When networking or configuration
copies require descriptor setup, it shows the launch plan with descriptor
placeholders as comments, followed by a command to run the same workcell
configuration. Otherwise it prints the direct bubblewrap invocation.
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
Process sharing does not change the selected network mode.
In host and offline network modes, a resolver under `/run/systemd/resolve`
(including Toolbx's `/run/host/run/systemd/resolve` layout) is preserved by
binding that directory read-only. This follows resolver file replacements while
keeping other `/run` contents hidden. Explicit resolver volumes take precedence.
Individual targets of `/etc/resolv.conf`, `/etc/hosts`, `/etc/host.conf`, and
`/etc/localtime` are preserved read-only when they live outside the exposed
system directories, as they commonly do in Toolbx. Their enclosing directories
remain hidden.
Configuration files backed by unlinked inodes, including a parent workcell's
generated resolver, are copied into read-only mounts so nested workcells can
preserve them too.
Individual file mounts follow in-place edits but not file replacements; copies
are snapshots. Private networking's generated resolver remains a launch-time
snapshot of search/domain and options settings.

Networking defaults to `--network private`. Each workcell gets its own network
namespace with outbound connectivity provided by a separate pasta process.
Automatic TCP and UDP forwarding is disabled in both directions, and the special
host loopback/gateway mappings are disabled. Workcell does not join or modify
Podman's networking namespaces or configuration.

Choose a mode explicitly with `--network MODE`:

| Mode | Connectivity |
| --- | --- |
| `private` (default) | Outbound networking through pasta; separate localhost. |
| `none` | Private loopback only; no external connectivity; pasta not required. |
| `host` | Share the enclosing network, including its localhost services. |

`--share-network` is an alias for `--network host`; it cannot be combined with
`--network`. Network mode is independent of `--share-processes` and
`--share-filesystem`.

In private mode, workcell supplies a read-only `/etc/resolv.conf` using pasta's
DNS forwarder at `169.254.1.1`, retaining the enclosing resolver's search/domain
and options settings. Pasta forwards DNS to the enclosing nameserver, including
a resolver listening on the enclosing loopback interface. The invoking UID, GID,
and hostname are preserved inside workcell. Bubblewrap creates the namespace
with the command paused; workcell attaches pasta to it, waits for network setup,
then releases the command. Workcell supervises both processes, returns the
command's exit status, and stops the sandbox if the helper exits unexpectedly.
Both processes are cleaned up when the command exits or workcell is terminated.

Pasta diagnostics go to a separate owner-readable log for each run under
`$XDG_STATE_HOME/workcell/logs`, or `~/.local/state/workcell/logs` when that
variable is unset or not absolute. This location uses the enclosing environment,
before workcell isolates the home directory. Startup failures and unexpected
helper exits report the log path and up to 20 recent lines (at most 8 KiB) in
the terminal. Command output and bubblewrap errors still go to the terminal.
Successful network-preflight logs are removed; failed preflight and actual-run
logs are retained, with no automatic pruning. Logging does not fix network
errors; it keeps helper diagnostics from interrupting interactive programs.

Private mode needs pasta on `PATH`, an accessible `/dev/net/tun`, a configured
nameserver in `/etc/resolv.conf`, and permission to create the required
namespaces. Workcell checks network setup before creating a persistent home or
running your command. It fails rather than falling back to host networking.
An enclosing workcell's minimal `/dev` hides `/dev/net/tun`, so nested private
networking needs that device exposed by the enclosing sandbox; alternatively,
use `--share-network` to use the enclosing workcell's network.

Private networking separates localhost; it does not block every service reachable
through host or LAN addresses. File-mounted Unix sockets are governed by filesystem
visibility, independently of network mode. Port publishing and destination
filtering are not implemented yet.

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
System configuration under `/etc` remains readable, and private networking
allows outbound connections. Explicit mounts and the compatibility options can
weaken isolation. Named capabilities and syscall filtering are deferred.

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
Filesystem and process examples use explicit host networking. Network checks
exercise host and offline modes, localhost separation, DNS/argv setup, and clear
private-mode startup failures. The real pasta supervisor is also tested in
loopback-only mode to verify identity, DNS/argv handling, and cleanup without a
TUN device. Tests requiring a TUN-backed private network are pending when the enclosing
environment lacks the helper, TUN device, or working namespace support.
Session checks cover entry without sudo, environment and namespace preservation,
concurrent sessions, stale records, PID namespace scoping, and cleanup.
To check actual internet access and DNS resolution on the host, run
`workcell -- curl --max-time 15 -I https://example.com`.

Run the tasty-bench startup benchmarks with Cabal:

```sh
cabal bench bench --benchmark-options='--timeout 5s'
```

The benchmarks measure wall-clock time for `--dryrun` and launching `/bin/true`,
including process startup and cleanup. Both use `--network host` so pasta is not
required. The live benchmark is skipped with a diagnostic if sandbox startup is
unavailable. Cabal supplies the built executable; set `WORKCELL` to an absolute
path to benchmark another binary.
