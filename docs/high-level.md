# cmake-wizard: user guide

A VSCode-CMake-Tools-like workflow for CMake projects in Emacs:
configure, build, pick a compiler ("kit"), pick a build type
("profile"), get `clangd` wired up automatically, and have
`projectile` know about all of it.

**Requires:** GNU Emacs 31.1+ and CMake 4.0+. Projects should declare
`cmake_minimum_required(VERSION 4.0)` or newer; older projects will
still mostly work but print a one-line warning.

## Installation

Clone this repo somewhere on your `load-path` and:

```elisp
(require 'cmake-wizard)
```

That pulls in every submodule. If you only want part of the suite
(say, just the configure/build engine without the projectile glue),
require the individual file instead, e.g. `(require 'cmake-wizard-compile)`.

Optional, for the parts of the suite that enhance them:

- [`cmake-mode`](https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el)
  for `CMakeLists.txt`/`.cmake` syntax highlighting and indentation (REQ-4.1).
- [`projectile`](https://github.com/bbatsov/projectile) for project
  detection and `projectile-configure-project`/`projectile-compile-project`.
- [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode) if you'd rather
  use it than the built-in `eglot`.

Nothing else is required - `eglot` and `project.el` ship with Emacs
29+/31.

`cmake-wizard-mode` (a minor mode providing the keybindings below) is
turned on automatically in `c-mode`, `c++-mode`, their tree-sitter
variants, and `cmake-mode` buffers that live inside a directory
containing a `CMakeLists.txt`.

## Core concepts (VSCode CMake Tools equivalents)

| cmake-wizard term | VSCode CMake Tools term | What it controls |
|---|---|---|
| **toolchain** | kit | Which compiler cmake configures with (`cmake-wizard-toolchains`) |
| **profile** | variant | `CMAKE_BUILD_TYPE` and other cache vars (`cmake-wizard-profiles`) |
| build directory | build directory | `<root>/build/<profile>-<toolchain>/`, out-of-source |

## Commands and keybindings

All bound in `cmake-wizard-mode-map` under the `C-c c` prefix:

| Key | Command | Does |
|---|---|---|
| `C-c c c` | `cmake-wizard-configure` | Run cmake's configure step |
| `C-c c b` | `cmake-wizard-build` | Build (configuring first if needed); with a prefix arg, prompts for a target |
| `C-c c m` | `cmake-wizard-compile` | Classic `M-x compile` front door: fills `compile-command` and lets you edit it in the minibuffer before running |
| `C-c c t` | `cmake-wizard-test` | Run the project's tests via `ctest` |
| `C-c c P` | `cmake-wizard-package` | Build distributable packages via `cpack` |
| `C-c c p` | `cmake-wizard-select-profile` | Switch Debug/Release/... |
| `C-c c k` | `cmake-wizard-select-toolchain` | Switch compiler/kit |
| `C-c c s` | `cmake-wizard-save-settings-to-dir-locals` | Persist the current profile/toolchain/compile-command into `.dir-locals.el` |

## Typical workflow

1. Open any file in a CMake project. `cmake-wizard-mode` turns on.
2. `C-c c k` to pick a toolchain (kit) if you don't want the default
   (Clang).
3. `C-c c p` to pick Debug/Release/etc. (defaults to Debug).
4. `C-c c b` - this configures (first time) and builds.
5. Once configured, `compile_commands.json` is symlinked into the
   project root and `eglot`/`lsp-mode` starts automatically in your
   open C/C++ buffers, so `clangd` sees the real flags cmake used.
6. Hit an error? `M-g n`/`M-g p` (`next-error`/`previous-error`) jump
   straight to it, for both compiler diagnostics and CMake's own
   `CMake Error at ...` messages.
7. `C-c c t` to run the project's tests, `C-c c P` to build a
   distributable package - both require the project's own
   CMakeLists.txt to opt in (see below).
8. Happy with your choices? `C-c c s` writes them to `.dir-locals.el`
   so the next person (or you, next week) gets the same setup for
   free.

### Running tests (REQ: "can I run tests with cmake?")

`C-c c t` / `cmake-wizard-test` runs `ctest --test-dir <build-dir>
--output-on-failure` - this requires the project's CMakeLists.txt to
call `enable_testing()` (or `include(CTest)`) and register tests with
`add_test(...)`; if it hasn't, `ctest` itself reports "No tests were
found!!!" and exits 0.

This intentionally does **not** go through `cmake --build --target
test`: that build target's name is generator-dependent ("test" on
Make/Ninja, "RUN_TESTS" on the Visual Studio or Xcode generators), so
it would silently stop working the moment you pick a different
toolchain. `ctest --test-dir` reads the build directory directly and
works the same way regardless of generator.

Customize with `cmake-wizard-ctest-executable` and
`cmake-wizard-ctest-arguments` (e.g. add `"-R" "my-test-regex"` or
`"-j" "8"`).

### Building packages (REQ: "can I build packages with cmake?")

`C-c c P` / `cmake-wizard-package` runs `cpack --config
<build-dir>/CPackConfig.cmake` - this requires the project's
CMakeLists.txt to call `include(CPack)`; which package formats
actually get produced (TGZ, ZIP, DEB, RPM, ...) is controlled by
`CPACK_GENERATOR` in the project itself, not by cmake-wizard. If
`include(CPack)` was never called, `cmake-wizard-package` fails fast
with a clear message instead of cpack's own, easy-to-miss "Cannot
find CPack config file" error.

Customize with `cmake-wizard-cpack-executable` and
`cmake-wizard-cpack-arguments` (e.g. `("-G" "ZIP")` to override the
project's default generator for a single run).

### Using plain `M-x compile` instead

Nothing here requires using the convenience commands. `cmake-wizard`
sets a sensible default `compile-command`, so `M-x compile` on its own
already prompts you with a ready-to-edit cmake invocation. Add
`-DBUILD_TESTING=ON` or anything else right there in the minibuffer -
no custom UI needed. `next-error`, the regexp parsing, and the
compile_commands.json wiring all work identically through this path.

## Picking a toolchain / cross-compiling to another architecture

```elisp
(setq cmake-wizard-toolchains
      '(("clang" . (:c-compiler "clang" :cxx-compiler "clang++"))
        ("gcc"   . (:c-compiler "gcc" :cxx-compiler "g++"))
        ;; Cross-compiling to arm64 from an x86_64 host, via a
        ;; standard CMake toolchain file - the usual way to target a
        ;; different architecture, since Emacs itself can't cross-compile:
        ("arm64-cross" . (:toolchain-file "~/toolchains/arm64-linux-gnu.cmake"))
        ("mips-cross" . (:toolchain-file "~/toolchains/mips-linux-gnu.cmake"
                          :extra-args ("-DCMAKE_SYSTEM_PROCESSOR=mips")))))
```

Then `C-c c t` and pick `arm64-cross`/`mips-cross`. Each toolchain gets
its own build directory, so switching back and forth never reuses a
stale `CMakeCache.txt` configured for a different compiler.

## Adding a custom profile

```elisp
(add-to-list 'cmake-wizard-profiles
             '("Coverage" . (("CMAKE_BUILD_TYPE" . "Debug")
                              ("CMAKE_CXX_FLAGS" . "--coverage -O0"))))
```

## Global cmake arguments (REQ-3.4-style override)

For a flag you always want, regardless of project, profile or
toolchain:

```elisp
(setq cmake-wizard-cmake-global-arguments '("-DBUILD_TESTING=ON"))
```

## `.dir-locals.el` examples (REQ-3.1/3.2)

Written by `C-c c s` / `cmake-wizard-save-settings-to-dir-locals`, or
by hand:

```elisp
;; .dir-locals.el at the project root
((nil . ((cmake-wizard-default-profile . "Release")
         (cmake-wizard-default-toolchain . "gcc")
         (compile-command . "cmake -S . -B build/Release-gcc -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_BUILD_TYPE=Release"))))
```

## projectile integration (REQ-4.3)

If `projectile` is loaded, cmake-wizard registers a `cmake-wizard`
project type keyed on `CMakeLists.txt` automatically - no extra setup.
`projectile-configure-project` and `projectile-compile-project` then
just work for any CMake project.

## LSP client choice

```elisp
(setq cmake-wizard-lsp-client 'eglot)   ; or 'lsp-mode, 'auto (default), 'none
```

## Scope: what this package deliberately does *not* do

- **CMakePresets.json** is not parsed. cmake-wizard's
  toolchain+profile model covers the same ground (compiler selection +
  cache variables) with plain Elisp customization; presets support
  could be layered on top later as another `cmake-wizard-toolchains`/
  `cmake-wizard-profiles` source if needed.
- No support for CMake older than 4.0, or for Emacs older than 31.1.

## Known limitations

- **`compile_commands.json` needs a single-config generator.** cmake
  only honors `CMAKE_EXPORT_COMPILE_COMMANDS` for Makefile/Ninja
  generators, not multi-config ones (Xcode, Visual Studio). If you add
  `-G Xcode`/`-G "Visual Studio ..."` to a toolchain's `:extra-args`,
  configure will still succeed but `cmake-wizard-compdb-sync` will
  silently find nothing to link and clangd won't get wired up - this
  is a cmake limitation, not something cmake-wizard can work around.
- **No pre-configure/post-build hooks yet.** There is only
  `cmake-wizard-configure-success-functions` (runs after a successful
  *configure*). There is nothing equivalent for "before configure" or
  "after build" (e.g. to run a test suite automatically after a
  build). This would be a natural, low-risk addition using the same
  hook pattern if you need it.
- **Profile/toolchain *definitions* are global; the current
  *selection* is per-project.** `cmake-wizard-set-profile`/
  `-set-toolchain` store the active choice per project root, so two
  projects never fight over which profile is active. But the lists
  of *available* profiles/toolchains
  (`cmake-wizard-profiles`/`cmake-wizard-toolchains`) are ordinary
  global `defcustom`s - adding a project-specific profile pollutes
  that list for every project unless you also override it via
  `.dir-locals.el` (possible, but you'll need to mark it as a safe
  local variable yourself; only `cmake-wizard-default-profile`,
  `cmake-wizard-default-toolchain`, `cmake-wizard-build-directory-name`
  and `cmake-wizard-cmake-global-arguments` are pre-marked safe).
- **Concurrent invocations rely on `compile.el`'s own prompt.** If you
  run `cmake-wizard-build` while a configure/build is still running
  for the same project, Emacs will ask (interactively) whether to kill
  the existing process, exactly as plain `M-x compile` does. There's
  no cmake-wizard-level queuing.
- **Switching profile/toolchain doesn't invalidate the clangd wiring.**
  `compile_commands.json` at the project root reflects whatever
  profile/toolchain you *last configured*, not whatever is currently
  selected. Re-run `cmake-wizard-configure` after switching if you
  want clangd to see the new flags.
- **Tested on macOS/Linux; the Windows symlink-fallback path is only
  unit-tested by mocking `make-symbolic-link` to fail**, not verified
  on an actual Windows machine. Likewise, cross-architecture toolchain
  files are a documented pattern, not something this suite has
  exercised against a real cross-toolchain.
