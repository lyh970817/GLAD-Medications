# Repository Guidelines

## Start Here

- `docs/analysis-pipeline.md` — how the analysis is structured, what it
  produces, and the traps that are easy to fall into. Read this before touching
  `src/` or `munge/`.
- `docs/project-status.md` — what state the analysis is in and what is still
  open.

Two things worth knowing before you run anything:

**Do not refit the models casually.** `cache()` in this project is a checkpoint,
not a memoisation gate — it cannot skip the computation above it. A full refit
takes about 2h20m. `src/02.regression.r` guards each fitting block with
`fit_or_cached()`, so a warm cache is reused automatically; expect
`Reusing cached model object: ...` in the log. Set `FORCE_REFIT=TRUE` only when
you genuinely want to refit.

**Products are not in the repository.** `results/`, `graphs/`, `cache/`,
`data/`, and `reports/` are all gitignored. Never commit their contents.
`results/medications_sig_final.xlsx` in particular is irreplaceable and not
reproducible by any current code.

## Project Structure & Module Organization

This R analysis project uses `ProjectTemplate` for loading data, libraries, helpers, and munging scripts. Core analysis scripts live in `src/`, currently regression work (`src/02.regression.r`) and plotting (`src/03.results_plot.r`). Data-changing preparation belongs in `munge/`, especially `munge/01.dat_clean.r`. Reusable functions belong in numbered `lib/` files. Raw data is expected under `data/`; generated objects go in `cache/`; outputs are written to `results/`, `graphs/`, `reports/`, or `manuscripts/`. Project configuration is in `config/global.dcf`; dependency state is in `renv.lock`.

## Build, Test, and Development Commands

- `direnv allow`: enter the Nix shell declared by `.envrc` and `shell.nix`.
- `nix-shell`: manually enter the R/system-library environment when not using direnv.
- `R -q -e 'renv::restore()'`: restore R packages from `renv.lock`.
- `R -q -e 'library(ProjectTemplate); load.project()'`: verify configuration, libraries, helpers, data loading, and munging.
- `Rscript src/02.regression.r`: regenerate result workbooks. Reuses cached model
  objects; only fits what is missing. Prefix with `FORCE_REFIT=TRUE` to refit.
- `Rscript src/03.results_plot.r`: regenerate result plots from cached models.
- `Rscript reports/compare_legacy_glmm_results.R`: rebuild the legacy-vs-GLMM
  comparison from the current workbooks.

Both `src/` scripts call `reload.project()` themselves, so they can be run
directly. Redirect output to `logs/` when running them unattended — they are
slow and produce no progress output while fitting.

Use `reload.project()` inside interactive R sessions after editing `lib/`, `munge/`, or config files.

## Coding Style & Naming Conventions

Follow the existing R style: two-space indentation, lowercase snake_case object names, and explicit helper functions for repeated logic. Keep numbered script prefixes (`01.`, `02.`, etc.) when order matters. Prefer tidyverse pipelines where they improve readability, but keep model code explicit. Add helpers to `lib/` rather than duplicating logic in `src/`. Put local overrides in `.envrc.local`.

## Testing Guidelines

There is no formal test suite. Treat `library(ProjectTemplate); load.project()` as the baseline smoke test before committing. For analysis changes, run the affected script and inspect artifacts in `results/` or `graphs/`. When changing data cleaning, verify column names, factor levels, and row counts before rerunning downstream analyses.

Before a long run, check that scripts still parse:

```
Rscript -e 'invisible(lapply(c(list.files("src", full.names=TRUE), list.files("lib", full.names=TRUE), list.files("munge", full.names=TRUE)), parse)); cat("OK\n")'
```

Keep ad hoc debugging scripts out of the repository root; `logs/` is gitignored and is the right place for run output.

## Commit & Pull Request Guidelines

Recent history uses Conventional Commit prefixes such as `feat:`, `fix:`, `chore:`, and `build:`. Keep commits focused, for example `fix: handle ggplot2 linewidth compatibility`. Pull requests should describe the analysis change, list commands run, mention affected outputs, and call out changes to `renv.lock`, `config/global.dcf`, or data assumptions. Include screenshots or file paths for changed plots.

## Security & Data Handling

The `data/`, `cache/`, `graphs/`, `results/`, and `manuscripts/` directories are output or sensitive-data locations and are mostly ignored. Do not commit raw participant data, generated caches, sensitive logs, or local environment files. If a dependency needs system libraries, update `shell.nix`; do not hard-code installation workarounds in analysis scripts.
