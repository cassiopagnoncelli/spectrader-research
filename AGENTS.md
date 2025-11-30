# Repository Guidelines

## Project Structure & Module Organization
- `R/`: core package code (signals, exits, risk, options, caching). Add new functions here and keep them documented.
- `rd/`: research assets and theory modules (e.g., `rd/theory/qboost`, `rd/analyses`, models, libraries). Place explorations and datasets here, not in `R/`.
- `dev/`: helper scripts; `update_deps.R` installs sibling tarballs (`qetl`, `fets`, `qboost`, `dtools`), and `spectrader/trades.R` bridges to Spectrader.
- `man/`, `DESCRIPTION`, `NAMESPACE`: generated from roxygen; edit comments in `R/` and run docs instead of hand-editing.
- `builds/`: packaged tarballs and local library installs; clean up old artifacts when regenerating.
- `docs/`: generated notes and reports; keep them reproducible via code in `R/` or `rd/`.
- `renv/`, `renv.lock`: pinned environment; use `renv::restore()` before running code.

## Build, Test, and Development Commands
- `Rscript -e "renv::restore()"` to sync dependencies (expects sibling package tarballs referenced in `dev/update_deps.R`).
- `make build` builds the package tarball into `builds/`; `make check` runs `R CMD check` on it (no manual).
- `make install` installs the built package into `builds/library` for local testing; `make clean` removes build artifacts.
- `make test` (or `make tests`) runs `devtools::test()` if tests exist.
- `make lint` runs `lintr`; `make style` formats with `styler`; `make docs` regenerates roxygen docs/NAMESPACE.

## Coding Style & Naming Conventions
- R code uses 2-space indents, tidyverse-friendly pipes, and `snake_case` for functions/objects.
- Keep lines ≤120 chars; aim for cyclomatic complexity ≤25 (see `.lintr`); avoid trailing noise in commits.
- Document every exported function with roxygen blocks; run `make docs` after changes to refresh `man/` and `NAMESPACE`.
- Favor vectorized operations and pure functions; return tidy structures that compose cleanly in pipelines.

## Testing Guidelines
- Use `testthat` (edition 3); place specs under `tests/testthat/test-<topic>.R` with clear fixtures.
- Seed randomness (`withr::with_seed`) and keep test data minimal; prefer deterministic snapshots over large dumps.
- Run `make test` before opening a PR; add coverage checks locally (e.g., `covr`) when touching critical paths.

## Commit & Pull Request Guidelines
- Write concise, present-tense commit messages that describe the change, mirroring existing history (e.g., “Adjust qel_cutoff for exit analysis”).
- PRs should explain motivation, summarize behavior changes, list test commands run, and note any renv or data impacts.
- Attach plots/screenshots for visual changes; link related issues or research notes in `rd/`.
- Keep diffs focused; update docs/tests alongside code and avoid committing built artifacts in `builds/` unless intentionally publishing.
