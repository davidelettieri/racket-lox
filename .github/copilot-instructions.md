# AI Coding Agent Instructions for `racket-lox`

These notes capture project-specific architecture, workflows, and conventions so an AI agent can be productive quickly. Keep edits incremental and preserve public interfaces unless the change is part of a deliberate refactor.

## Big Picture
- Goal: A (currently incomplete / experimental) Racket implementation of the Lox language from the Crafting Interpreters book.
- Two worlds: (1) `racket-lox` Racket language (macros + reader + scanner + tests). (2) Upstream reference implementations under `craftinginterpreters/` (C "clox", Java "jlox", plus Dart test harness).
- Racket language entry point: using `#lang racket-lox` relies on `lang/reader.rkt` which wraps user code inside a module targeting the language defined by `main.rkt` + `lox.rkt`.
- Current scope: Token scanning and a handful of macros for variable definition/assignment, printing, addition, simple blocks. Parsing / evaluation beyond these primitives is not yet implemented.

## Core Files & Responsibilities
- `main.rkt`: Supplies a custom `#%module-begin`, re-exporting Racket bindings plus everything from `lox.rkt`.
- `lox.rkt`: Macro layer implementing minimal Lox-like constructs (`lox-define-var`, `lox-assign`, `lox-print`, `lox-add`, `lox-block`, `lox-var-value`, `lox-declarations`). Runtime error helper `lox-runtime-error` exits with code 70; keep behavior stable for tests.
- `lang/reader.rkt`: Custom reader injecting forms into `(module anonymous-module racket-lox ...)`. Avoid breaking shape; tools rely on this to treat source as a language module.
- `lang/helpers.rkt`: Utility macro: `while` implemented via recursive local loop.
- `lang/scanner.rkt`: Tokenization logic. Exports `scan-tokens`, `scan-token`, and `token` struct. Builds tokens by recursive accumulation then reverses. Known limitation: keywords map defined (`keywords`) but not yet used; future expansion would map identifier lexemes.
- `lang/scanner-tests.rkt`: Rackunit tests asserting token types and lexemes for punctuation/operators/comments.

## Testing & Workflows
- Ignore the craftinginterpreters subfolder.
- Validate builds:
  - `find lang -name '*.rkt' -exec raco make {} +` to compile all Racket source files.
- Test files are all files with `-tests.rkt` suffix.
- Run local Racket tests: `raco test -x -p racket-lox`
- Cross-reference upstream book tests via `run-tests.sh` which executes Dart harness: `craftinginterpreters/tool/bin/test.dart chap08_statements --interpreter racket`. This expects further interpreter features; many chapters are not yet implemented here. Run this script only if directly asked.
- When adding scanner features (identifiers, strings, numbers), mirror patterns from book and extend existing tests in `lang/scanner-tests.rkt` before implementing logic.

## Conventions & Patterns
- Macros instead of functions for language surface constructs (e.g., `lox-add` wraps to `lox-add-impl` capturing source line for errors). Preserve line propagation for meaningful error messages.
- Error signaling: use `lox-runtime-error` to print to `current-error-port` and exit (code 70). Maintain format: message line on stderr followed by `[line <n>] in script`.
- Tokens: `struct token (type lexeme literal line column) #:transparent`. Do not reorder fields; tests rely on accessors like `token-type` / `token-lexeme`.
- Scanner style: Single-character tokens handled via `case`. Multi-character lookahead uses `match` which peeks and consumes. Comments: line comments start with `//` and are skipped until newline.
- Numbers: README notes forced flonum representation differences vs book reference; be consistent if adding numeric literal scanning.

## Extending Safely
- Add new tokens: update `scan-token` with additional branches; extend tests first.
- Keyword handling: integrate `keywords` map when implementing identifiers; return token type from map or generic IDENTIFIER.
- Additional language constructs: follow existing macro naming `lox-<action>` and provide small runtime helpers when necessary.
- Keep public `provide` lists stable unless deliberately expanding; update `main.rkt` exports if adding new user-visible bindings.

## External Dependencies
- Dart + Racket environment setup via `scripts/install-dependencies.sh` (now caches Dart .deb under `.cache`). Keep script idempotent.
- Dart test harness is an external workflow; avoid coupling core interpreter logic directly to Dart tooling.

## Typical Agent Tasks Examples
- Implement identifier scanning: modify `scan-token` fallback to detect alphabetic sequences, consult `keywords`, create tokens; add rackunit tests for types and lexemes.
- Add numeric literal scanning: accumulate digits + optional fractional part; convert to flonum (`string->number` with `#i` prefix if needed) and set literal field.
- Add unit tests for new scanner features: extend `lang/scanner-tests.rkt` with representative cases covering edge conditions.

## Style & PR Guidance
- Prefer small focused PRs (scanner feature + tests). Include before/after token sequence examples in description.
- Preserve existing error message shape and exit codes unless escalating to richer diagnostics (justify in PR).

Provide feedback if any section is unclear or if deeper details (e.g., planned parser architecture) should be documented.
