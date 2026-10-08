# cmake-wizard: low-level design

This document explains *how* cmake-wizard works internally. For usage,
see `high-level.md`.

Targets: GNU Emacs 31.1+ only, CMake 4.0+ only.

## Module graph

```
cmake-wizard-project.el        (project discovery, per-root state, build dirs, version checks)
        ^
        |
cmake-wizard-profile.el   cmake-wizard-toolchain.el     (both depend only on -project)
        ^                       ^
        \_______________________/
                    |
          cmake-wizard-compile.el        (the execution engine)
                    ^
      ______________|_______________________________
     |              |              |                |
cmake-wizard-   cmake-wizard-  cmake-wizard-   cmake-wizard-
compdb.el       lsp.el         ctest.el        cpack.el
     ^   (compdb/lsp hook into -compile's success hook;
     |    ctest/cpack just reuse `cmake-wizard--run')
     |
cmake-wizard-dir-locals.el   cmake-wizard-projectile.el   (independent leaves)
                    ^
          ___________|
                    |
             cmake-wizard.el          (facade: requires everything, minor mode)
```

Every file is independently `require`-able; `cmake-wizard.el` is a
convenience that pulls in the whole suite. This is a deliberate choice
over N separate repositories: it keeps `Package-Requires` headers
accurate per-file while letting `(require 'cmake-wizard)` be the
one-line "I want it all" entry point, matching how large multi-file
packages like `lsp-mode` or `projectile` are organized.

## Per-project state, not one global struct

The original prototype kept a single global `cmake-wizard--session`
struct, so only one project could ever be tracked across an Emacs
session. `cmake-wizard-project.el` replaces that with a hash table,
`cmake-wizard--project-state`, keyed by canonicalized project root
(absolute, slash-terminated), each value a plist (`:profile`,
`:toolchain`, ...). `cmake-wizard-project-get`/`cmake-wizard-project-put`
are the only access points, so adding a new piece of per-project state
(from any module) never requires touching a shared struct definition.

Root canonicalization (`cmake-wizard--canonical-root`) matters:
without it, `~/proj/` and `/home/user/proj` would be treated as two
different projects and silently diverge in profile/toolchain state.

## Project discovery

`cmake-wizard-project-root` tries, in order:

1. `projectile-project-root` (soft dependency - only called if
   `(fboundp 'projectile-project-root)`)
2. `project-current`/`project-root` from the built-in `project.el`
3. `locate-dominating-file` walking up for `CMakeLists.txt`

This order means a user who has `projectile` loaded gets whatever
notion of "project root" `projectile` already uses (respecting
`.projectile` files, VCS roots, etc.), but the package degrades to a
plain directory walk with zero extra dependencies.

## The execution engine: reusing `compile.el`

This is the central design decision (see REQ-1.1). Instead of
hand-rolling `start-process` + a bespoke buffer (as the prototype
did), `cmake-wizard--run` is a two-line wrapper around
`compilation-start`:

```elisp
(defun cmake-wizard--run (root command)
  (let ((default-directory root))
    (compilation-start command nil (lambda (_mode) (cmake-wizard--buffer-name root)))))
```

This single call gives us, for free, exactly what REQ-1 asks for:
asynchronous execution, a dedicated `*cmake-wizard:<root>*`
compilation-mode buffer (named from ROOT's full, abbreviated path -
*not* `cmake-wizard-project-name`, which is just a directory basename
and would collide for e.g. two unrelated projects both named "app"),
and `next-error`/`previous-error` navigation using Emacs' built-in
`gnu` compiler-error regexp plus a `cmake-wizard` regexp this package
registers for CMake's own `CMake Error/Warning at FILE:LINE (...):`
diagnostics (see `cmake-wizard--compilation-regexp` and
`cmake-wizard--register-compilation-regexp`, which pushes into the
*global* `compilation-error-regexp-alist-alist`/`-alist` once at load
time - so even a hand-typed `M-x compile` benefits).

### Extensibility hook, not a callback parameter

A naive design would have `cmake-wizard-configure` take an
`on-success` callback. That would only fire when *our* function
started the process - not when `projectile-configure-project` or a
plain `M-x compile` runs the exact same command string. Instead,
`cmake-wizard-compile.el` adds one *global* function,
`cmake-wizard--compilation-finish-handler`, to Emacs' own
`compilation-finish-functions`. It recognizes a finished buffer as
"one of ours" by checking whether its `compile-command` contains the
fingerprint `cmake-wizard--configure-marker`
(`"CMAKE_EXPORT_COMPILE_COMMANDS=ON"`, which every cmake-wizard
configure command includes), and if so runs the public abnormal hook
`cmake-wizard-configure-success-functions` with the project root.

`cmake-wizard-compdb.el` and `cmake-wizard-lsp.el` hook into
`cmake-wizard-configure-success-functions` with explicit `DEPTH`
values (`-10` for the compdb symlink, `10` for starting the LSP
client) rather than relying on load-order/`append` semantics, so the
ordering (symlink *before* starting clangd) is correct regardless of
which modules happen to be loaded. Third-party code can hook into the
same variable.

Net effect: the compdb-symlink-then-start-clangd pipeline runs
identically whether the configure command was launched by
`cmake-wizard-configure`, by projectile's `projectile-configure-project`
(cmake-wizard-projectile.el's `:configure` function just *returns* the
command string; projectile runs it through its own `compile`-based
plumbing), or by a user who typed the same `cmake ...` command into a
plain `M-x compile`.

### A second, private hook for "configure, then build"

`cmake-wizard-build` auto-configures when there's no build directory
yet, then builds once that configure succeeds. This chaining does
*not* use the public `cmake-wizard-configure-success-functions` hook:
an earlier version added a one-shot lambda there and had it
`remove-hook` itself when it ran - but that removal only happens on
*success*. If the configure fails, the lambda is never invoked and
never removed, so it sits on the *global* hook list forever and later
fires against the next unrelated project's successful configure,
silently triggering a bogus build somewhere else. (Confirmed with a
reproduction before fixing it - see the test
`cmake-wizard-test-build-chain-does-not-leak-into-public-hook`.)

The fix, in `cmake-wizard--configure-then-build`: attach the one-shot
function directly to the *buffer-local* `compilation-finish-functions`
of the one compilation buffer this particular configure started
(`(add-hook 'compilation-finish-functions fn nil t)`), and have it
`remove-hook` itself unconditionally, whether the message matches
`"finished"` or not. Scoped to a single buffer that only ever finishes
once, it can't leak across projects and can't survive a failure.

### Command strings, and why only *values* are shell-quoted

`cmake-wizard-configure-command`/`-build-command` build a single shell
command string (what `compile.el` expects). An earlier version
shell-quoted every token with `shell-quote-argument`, which seems
safe - until you notice that on non-Windows systems
`shell-quote-argument` backslash-escapes every character outside
`[-0-9a-zA-Z_./\n]`, which includes `=` and `+`. That turned
`-DCMAKE_BUILD_TYPE=Debug` into `-DCMAKE_BUILD_TYPE\=Debug` and
`clang++` into `clang\+\+` - harmless to the shell, but it broke the
marker-substring match above and made the minibuffer-edited
`compile-command` (REQ-3.3) look broken to a human.

The fix: only shell-quote the parts that can plausibly contain spaces
or shell metacharacters - directory paths (`-S`/`-B`/`--build`
arguments), a `--target` name, a `CMAKE_TOOLCHAIN_FILE` path, and
cache-variable *values* (not the `-DVAR=` prefix). Everything else
(flag names, `=`, compiler names like `clang++`) is emitted literally.
Tests that need to inspect individual arguments parse the command back
into a list with `split-string-and-unquote` rather than
pattern-matching the raw string, since quoting style is otherwise an
implementation detail.

## Build directory layout

`cmake-wizard-build-directory` (in `-project.el`) namespaces build
trees by **both** profile and toolchain:
`<root>/build/<profile>-<toolchain>/` (e.g. `build/Debug-clang/`).
Namespacing by profile alone (as VSCode CMake Tools effectively does
with its default build dir) is not enough here, because
`cmake-wizard-select-toolchain` lets a user switch compilers on the
same profile at any time - reusing a `CMakeCache.txt` configured for a
different `CMAKE_C_COMPILER` is a real cmake error, not just a style
preference. The layout is pluggable via
`cmake-wizard-build-directory-function`.

## compile_commands.json wiring (REQ-2.2/2.3)

Every configure command unconditionally passes
`-DCMAKE_EXPORT_COMPILE_COMMANDS=ON` (REQ-2.2); `clangd` only looks for
`compile_commands.json` in the project root or an ancestor, not inside
an arbitrary build directory, so `cmake-wizard-compdb-sync`
(hooked at depth `-10`) copies/symlinks it there after every
successful configure. Symlinking is tried first; `make-symbolic-link`
failing with `file-error` (expected on Windows without Developer Mode
or admin rights) falls back to `copy-file`. This is the one place
cmake-wizard does OS-conditional work, and it is a graceful runtime
fallback rather than a `system-type` branch, so it also covers less
common cases (restricted filesystems, sandboxes) uniformly.

## LSP wiring (REQ-2.1)

`cmake-wizard-lsp.el` only ever calls `eglot-ensure` or `lsp-deferred`
- never anything clangd-specific - because both of those packages
already discover `clangd` via `compile_commands.json` in the project
root on their own. cmake-wizard's job stops at making sure that file
exists where they'll look; `cmake-wizard-lsp-client` just picks which
of the two (or neither) to drive, resolved through `fboundp` checks so
`lsp-mode` can be entirely absent without error (`eglot` is always
present since it's built into Emacs 29+).

## Running tests and building packages: avoid build-target names

`cmake-wizard-ctest.el` and `cmake-wizard-cpack.el` both have an
obvious-looking alternative that was deliberately rejected: calling
`cmake --build <build-dir> --target test` / `--target package`
instead of invoking `ctest`/`cpack` directly. That alternative is
fragile across generators:

- CTest's build-system-integration target is named `test` under
  Make/Ninja but `RUN_TESTS` under the Visual Studio and Xcode
  generators - a hard-coded `--target test` silently breaks the moment
  someone picks a different toolchain/generator, which this package
  explicitly supports (`cmake-wizard-toolchains`).
- `ctest --test-dir <build-dir>` reads `CTestTestfile.cmake` straight
  out of the build directory and behaves identically regardless of
  which native build tool produced it - confirmed empirically against
  a real build before writing this.
- Likewise, `cpack --config <build-dir>/CPackConfig.cmake` works from
  any `default-directory`, so `cmake-wizard-package` doesn't need to
  special-case anything either.

Both commands reuse `cmake-wizard--run` from the execution engine, so
they get the same dedicated-buffer/`next-error` treatment as configure
and build; `cmake-wizard-package` additionally pre-checks for
`CPackConfig.cmake` so a project that never called `include(CPack)`
gets an immediate, actionable `user-error` instead of having to go
read cpack's own error text inside the compilation buffer.

## Soft dependencies, and why that keeps tests honest

`projectile` and `lsp-mode` are never hard-required. Every call site
checks `fboundp` first. This is not just graceful degradation for
users - it is what lets the test suite exercise
`cmake-wizard-projectile.el` and the `lsp-mode` branch of
`cmake-wizard-lsp.el` *without installing either package*: in the real
test environment neither is present, so the "unavailable" branches are
exercised for free, and the "available" branches are exercised by
`cl-letf`-stubbing exactly the one or two functions each module calls
(`projectile-register-project-type`, `eglot-ensure`). No test needs
network access or a package manager.

## `.dir-locals.el` persistence (REQ-3.1/3.2/4.4)

`cmake-wizard-dir-locals.el` treats a project's `.dir-locals.el` as
data: `cmake-wizard--read-dir-locals`/`--write-dir-locals` round-trip
it through `read`/`pp`, and `cmake-wizard--dir-locals-merge` only
touches the keys it's given within the targeted mode-class (`nil` for
"all modes"), leaving unrelated mode-classes and unrelated variables
within the same mode-class untouched. `cmake-wizard-save-settings-to-dir-locals`
writes the current profile, toolchain, and the fully-expanded configure
command (as `compile-command`) - so a fresh Emacs opening the project
later gets exactly the same build without re-selecting anything.
