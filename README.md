# cmake-wizard

**A CMake workflow for Emacs, in the spirit of VSCode's CMake Tools:**
configure, build, test, and package a CMake project without leaving
Emacs or hand-typing `cmake` invocations - while still being 100%
compatible with plain `M-x compile` if that's all you want.

- Async configure/build/test/package, each in its own dedicated
  `compile.el` buffer - `next-error`/`M-g n` jumps straight to the
  failing line, for both compiler errors and CMake's own diagnostics.
- Pick a compiler ("toolchain"/kit) and a build type ("profile") from
  a list; switch anytime without clobbering your other build.
- `compile_commands.json` is wired up automatically so `eglot`
  (or `lsp-mode`) and `clangd` see the exact flags cmake used - no
  manual symlinking.
- `projectile` support, `.dir-locals.el` persistence, and `ctest`/
  `cpack` integration that doesn't break when you change toolchains.

**Requires:** GNU Emacs **31.1+** and CMake **4.0+**. Nothing else -
`eglot` and `project.el` ship with Emacs itself.

## Installation

Clone this repo somewhere and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/cmake-wizard")
(require 'cmake-wizard)
```

Or, with `use-package`:

```elisp
(use-package cmake-wizard
  :load-path "/path/to/cmake-wizard")
```

Or, with `straight.el`, pointing at a local checkout:

```elisp
(straight-use-package '(cmake-wizard :local-repo "/path/to/cmake-wizard"))
```

That's it - just `(require 'cmake-wizard)` loads the whole suite.
Nothing else needs installing, though three *optional* packages make
things nicer if you have them:

| Package | What it adds |
|---|---|
| [`cmake-mode`](https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el) | Syntax highlighting/indentation for `CMakeLists.txt` and `.cmake` files |
| [`projectile`](https://github.com/bbatsov/projectile) | Project detection; `projectile-configure-project`/`-compile-project` wired to cmake-wizard automatically |
| [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode) | Alternative to the built-in `eglot` for talking to `clangd` |

cmake-wizard detects at load time whether these are installed and
degrades gracefully if they aren't - you don't need to configure
anything either way.

## Getting started

Open any file inside a CMake project (anywhere under a directory
containing `CMakeLists.txt`). `cmake-wizard-mode` turns itself on
automatically in C, C++, and `cmake-mode` buffers, which gives you the
keybindings below. Then:

1. `C-c c b` - **build**. First time, this configures (runs `cmake -S
   . -B build/Debug-clang ...`) and then builds; after that it just
   builds. Watch it happen live in the `*cmake-wizard:...*` buffer.
2. Once configured, open any `.c`/`.cpp` file in the project: `eglot`
   starts automatically and `clangd` already knows your project's real
   include paths and defines - no setup needed.
3. See an error? `M-g n` / `M-g p` (`next-error`/`previous-error`)
   jumps to it - works for both compiler errors and `CMake Error at
   ...:` messages.
4. `C-c c t` runs your tests (`ctest`); `C-c c P` builds a
   distributable package (`cpack`) - if your `CMakeLists.txt` has
   opted into them (see below).

That's the whole on-ramp. Everything else is picking a different
compiler or build type, which is one keystroke each.

## How do I actually run a cmake command? (three ways)

**1. The dedicated commands below**, by keybinding or by name
(`M-x cmake-wizard-build`, no keybinding required). This is the
"VSCode button" experience - pick a toolchain/profile from a list,
press one key, watch the output.

**2. Plain `M-x compile`.** cmake-wizard keeps the buffer-local
`compile-command` variable pointed at the right `cmake ...` invocation
for your current toolchain/profile. So `M-x compile` on its own
already shows you a ready-to-run (and ready-to-edit) cmake command in
the minibuffer - add `-DBUILD_TESTING=ON` or anything else right there
and hit `RET`. `C-c c m` (`cmake-wizard-compile`) is just this, done
for you.

**3. `projectile`**, if you use it. `projectile-configure-project`
and `projectile-compile-project` call cmake-wizard automatically; no
extra setup.

All three run through the exact same engine (Emacs' own
`compilation-start`), so output parsing, `next-error`, and the
`clangd`/`compile_commands.json` wiring behave identically regardless
of which one you used.

## Commands and keybindings

Everything lives under the `C-c c` prefix (`cmake-wizard-mode-map`):

| Key | Command | Does |
|---|---|---|
| `C-c c c` | `cmake-wizard-configure` | Run cmake's configure step |
| `C-c c b` | `cmake-wizard-build` | Build (configures first if needed). With a prefix arg (`C-u`), prompts for a specific target |
| `C-c c m` | `cmake-wizard-compile` | Classic `M-x compile` front door: fills `compile-command`, lets you edit it before running |
| `C-c c t` | `cmake-wizard-test` | Run the project's tests via `ctest` |
| `C-c c P` | `cmake-wizard-package` | Build distributable packages via `cpack` |
| `C-c c p` | `cmake-wizard-select-profile` | Switch build type: Debug/Release/RelWithDebInfo/... |
| `C-c c k` | `cmake-wizard-select-toolchain` | Switch compiler ("kit"): clang/gcc/a cross-toolchain |
| `C-c c s` | `cmake-wizard-save-settings-to-dir-locals` | Write the current profile/toolchain/compile-command to `.dir-locals.el` so they persist |

**VSCode CMake Tools users:** "toolchain" here is what VSCode calls a
**kit**, and "profile" is what VSCode calls a **variant**. Same ideas,
different names.

## Running tests and building packages

Both require your `CMakeLists.txt` to opt in - cmake-wizard doesn't
add tests or packaging config for you, it just runs the tools:

```cmake
# for `C-c c t' / cmake-wizard-test
enable_testing()
add_test(NAME my_test COMMAND my_test_binary)

# for `C-c c P' / cmake-wizard-package
include(CPack)
```

`cmake-wizard-test` runs `ctest --test-dir <build-dir>
--output-on-failure` - deliberately *not* `cmake --build --target
test`, because that build target is named `test` under Make/Ninja but
`RUN_TESTS` under Visual Studio/Xcode, which would silently break the
moment you switch toolchains. `ctest --test-dir` doesn't have that
problem.

`cmake-wizard-package` runs `cpack --config
<build-dir>/CPackConfig.cmake`; if `include(CPack)` was never called,
you get an immediate, readable error instead of hunting through cpack
output to figure out why it failed.

## Customizing your toolchains and profiles

```elisp
(setq cmake-wizard-toolchains
      '(("clang" . (:c-compiler "clang" :cxx-compiler "clang++"))
        ("gcc"   . (:c-compiler "gcc" :cxx-compiler "g++"))
        ;; Cross-compiling to another architecture, via a standard
        ;; CMake toolchain file:
        ("arm64-cross" . (:toolchain-file "~/toolchains/arm64-linux-gnu.cmake")))
      cmake-wizard-default-toolchain "clang")

(add-to-list 'cmake-wizard-profiles
             '("Coverage" . (("CMAKE_BUILD_TYPE" . "Debug")
                              ("CMAKE_CXX_FLAGS" . "--coverage -O0"))))

;; A flag you always want, regardless of project/profile/toolchain:
(setq cmake-wizard-cmake-global-arguments '("-DBUILD_TESTING=ON"))

;; Which LSP client to drive: 'eglot, 'lsp-mode, 'auto (default), 'none
(setq cmake-wizard-lsp-client 'eglot)
```

`C-c c k`/`C-c c p` then just show up in `completing-read` with
whatever you've added. Each toolchain+profile combination gets its
own build directory, so switching back and forth never corrupts a
`CMakeCache.txt` configured for a different compiler.

## Making a choice stick: `.dir-locals.el`

`C-c c s` writes your current profile/toolchain/compile-command into
the project's `.dir-locals.el`, so the next person (or you, next
week) gets the same setup automatically - no re-selecting anything.
You can also write it by hand:

```elisp
;; .dir-locals.el at the project root
((nil . ((cmake-wizard-default-profile . "Release")
         (cmake-wizard-default-toolchain . "gcc"))))
```

## Learn more

- [docs/high-level.md](docs/high-level.md) - the full user guide:
  every customization option, known limitations, and the full
  rationale for each design choice (e.g. why `ctest`/`cpack` are
  invoked the way they are).
- [docs/low-level.md](docs/low-level.md) - internals, for anyone
  extending the package or just curious how it works.

## Package layout

One repo, several independently-`require`-able files:

- `cmake-wizard.el` - facade, minor mode, keybindings
- `cmake-wizard-project.el` - project discovery, per-project state, build dirs
- `cmake-wizard-profile.el` - build-type ("variant") switching
- `cmake-wizard-toolchain.el` - compiler ("kit") selection
- `cmake-wizard-compile.el` - the `compile.el`-backed configure/build engine
- `cmake-wizard-compdb.el` - `compile_commands.json` -> project-root wiring
- `cmake-wizard-lsp.el` - `eglot`/`lsp-mode` glue
- `cmake-wizard-ctest.el` - `ctest` integration
- `cmake-wizard-cpack.el` - `cpack` integration
- `cmake-wizard-projectile.el` - `projectile` glue
- `cmake-wizard-dir-locals.el` - `.dir-locals.el` persistence

Only want part of the suite? `(require 'cmake-wizard-compile)` on its
own works fine, with no projectile/LSP glue pulled in.

## Testing (for contributors)

```sh
make unit-test   # ERT, mocks all processes and optional deps
make e2e-test    # real cmake + clang + ctest + cpack against test/fixtures
make test        # both
```

`make e2e-test` skips itself automatically if `cmake`/`clang`/`ctest`/
`cpack` aren't on `PATH`.
