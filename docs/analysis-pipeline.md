# Analysis Pipeline

Reference for the structure of the analysis, its outputs, and the traps that are
easy to fall into. Written for someone (or some agent) picking the project up
cold.

## Two parallel pipelines

The project runs **two analyses side by side** on the same data. This is
deliberate — the second was not replaced by the first, so that the two can be
compared.

| | GLMM (longitudinal) | Legacy (aggregate) |
|---|---|---|
| Unit of analysis | one row per participant × medication episode | one row per participant |
| Random effects | participant random intercept, `(1 \| ID)` | none |
| Fitting helper | `fit_glmm()` in `lib/02.fit_funs.r` | `fit_prop()` / `fit_hurdle()` |
| Model objects | `glmm_models`, `glmm_models_cov` | `sef_models`, `eff_models`, `sef_models_compete`, `eff_models_compete` |
| Data frame | `dat_long_glmm` | `dat_nonlong_sef`, `dat_nonlong_eff` |

Both live in `src/02.regression.r`; the legacy block starts under the comment
`Additional non-longitudinal models`.

## Outcomes

The two pipelines model **different outcome sets**, because the legacy outcomes
are per-person summaries with nothing for a within-person model to use.

GLMM (`glmm_deps`), four outcomes:

| Outcome | Family | Note |
|---|---|---|
| `effectiveness` | ordinal `clmm` | 3 observed levels; the only true ordinal fit |
| `remission` | binomial `glmer` | genuinely binary; `munge/01.dat_clean.r` wraps it in `factor(..., ordered = TRUE)`, but that is a no-op on a 2-level factor |
| `n_se` | Poisson `glmer` | count of side-effect checklist items; effect is a rate ratio |
| `stopped_due_to_se` | binomial `glmer` | |

Legacy, four outcomes: `se_rating` (side-effect family) and `ben_rating`,
`n_best`, `first_imprv` (effectiveness family).

`fit_glmm()` dispatches in this order: binary factor → binomial; ordered factor
→ `clmm`; explicit family → `glmer`; non-negative integer → Poisson; else
`lmer`.

## Predictors — one model per predictor

**Each focal predictor is fitted in its own separate model.** `indeps_list` is a
list of vectors and `fit_model()` maps over it, so you get
`effectiveness ~ sex + (1|ID)` and `effectiveness ~ bmi + (1|ID)` as distinct
fits — never one joint multivariable model.

This matters for interpretation: predictors cannot compete for variance or
"absorb" each other's signal, because they never appear in the same formula.

Three **cross-outcome predictors** (`n_se`, `se_rating`, `stopped_due_to_se`)
are added only for the `effectiveness` and `remission` models, via
`get_indeps_list_for_dep()`. They are stored as three separate one-element
vectors, so they are three extra models, not a combined one. The legacy
equivalents are named `mean_n_se`, `se_rating`, and `intolerance`.

## Adjusted models

Adjustment set: `sex`, `start_age`, `n_relatives`, `cumulative_med_count`,
`phq9`. These are **covariate-only** — `get_adjusted_indeps_list_for_dep()`
strips them from the focal predictor list so a variable never competes with
itself, and `drop_covariate_terms()` removes them from the output tables
(matching on prefix, so expanded factor terms like `sexFemale` are caught too).

They remain in the fitted model; they are only absent from the reported rows.

## Products

Everything below is **gitignored**. None of it is in the repository.

### `results/` — workbooks written by `src/02.regression.r`

| File | Contents |
|---|---|
| `glmm_medications.xlsx` / `_sig.xlsx` | GLMM unadjusted, all rows / significant only |
| `glmm_medications_cov.xlsx` / `_sig_cov.xlsx` | GLMM adjusted |
| `medications.xlsx` / `_sig.xlsx` | legacy unadjusted |
| `medications_cov.xlsx` / `_sig_cov.xlsx` | legacy adjusted |
| `medications_sig_final.xlsx` | **irreplaceable — see below** |

`medications_sig_final.xlsx` (dated 2025-12-04) is the only surviving copy of
the pre-GLMM legacy results for the `Occurence of remission`, `Average
effectiveness`, `Mean number of side effects`, and `Treatment discontinuation`
outcomes. No code in the repository reproduces it — the current legacy pipeline
does not model remission or effectiveness at all. It is the source of the
legacy figures used in the GLMM-vs-legacy comparison. **Do not delete it.**

### `graphs/` — plots written by `src/03.results_plot.r`

The four GLMM outcomes are split into two families for plotting
(`glmm_eff_deps` = effectiveness + remission, `glmm_sef_deps` = n_se +
stopped_due_to_se), which is why panels carry different numbers of markers:

| Family | Outcomes plotted | Markers per row |
|---|---|---|
| `glmm_all*` | all four GLMM outcomes | 4 |
| `glmm_eff*` | Effectiveness, Remission | 2 |
| `glmm_sef*` | N of Side Effects, Stopped due to SE | 2 |
| `eff*` | Benefit rating, N best aspects, First improvement | 3 |
| `sef*` | Side effect severity rating | 1 |

Files with `_cov` or `_compete` are the adjusted variants. Domain-grouped
panels (`_Demographics_Lifestyle`, `_Psychiatric_History`,
`_Somatic_Comorbidities`, `_Side_Effects`) come from `save_grouped_plots()` and
partition **predictors**, not outcomes.

There is no `Side_Effects` panel for the side-effect outcome families, because
no side-effect predictors are injected into those models. `save_grouped_plots()`
returns `NULL` for that combination, which is why the run log ends with
`$Side_Effects NULL` four times. That is expected, not an error.

Two files predate the current pipeline and cannot be regenerated:
`cor_plot.pdf` and `cor_plot_n_relatives_bin.pdf` came from
`src/01.descriptives.r`, deleted in commit `b03ea78`.

### `reports/` — local only, gitignored in full

`compare_legacy_glmm_results.R` builds `legacy_glmm_comparison.md` plus three
CSVs, comparing the two pipelines **by broad domain**, not outcome-to-outcome.
Filtering its output for a specific legacy outcome like `Remission` returns
nothing, because the current legacy pipeline no longer produces it — that is a
property of the comparison's design, not a bug.

## Traps

**`cache()` does not prevent recomputation.** Every call in this project is the
bare `cache("name")` form with no `CODE=` argument. Read the ProjectTemplate
source: without `CODE`, it only hashes whatever is already in the global
environment and writes it to disk. It is a checkpoint, not a memoisation gate,
and it cannot skip the assignment above it.

Re-running `src/02.regression.r` therefore used to refit everything — about
2h20m — regardless of what was in `cache/`. `fit_or_cached()` now guards each
fitting block: because `reload.project()` restores cached objects (the config
sets `cache_loading: TRUE`), already-fitted models are reused and only missing
ones are fitted. Set `FORCE_REFIT=TRUE` to override.

Expect `Reusing cached model object: ...` in the log. If you see `Fitting: ...`
for all six, the cache is cold and you are in for a multi-hour run.

**Namespace masking.** `plyr` is loaded after `dplyr` via `config/global.dcf`,
so `plyr::mutate` masks `dplyr::mutate`. This silently broke numeric coercion
of predictors once, leaving ordinal polynomial contrast terms (`.L`, `.Q`,
`.C`) in the output. Calls in `src/02.regression.r` are explicitly qualified as
`dplyr::mutate()` for this reason — keep them qualified.

**Label lookups need the legacy names.** The cached `labels` object does not
contain `mean_n_se` or `intolerance`. Both `src/02.regression.r` and
`src/03.results_plot.r` add them via a local `labels_extra` block. Omitting it
silently drops those predictors from grouped plots rather than erroring.

**Plot x-axis is capped at 2.** `lib/07.plot_funcs.r` uses
`coord_cartesian(xlim = c(NA, 2), clip = "off")`. Ten confidence intervals
across the current workbooks exceed that bound and are drawn running off the
panel edge with no truncation indicator; two of them are statistically
significant. Deliberate zoom, but worth knowing before reading a figure.

## Open questions

These are unresolved analysis decisions, not defects.

- **Cross-outcome predictor asymmetry.** `n_se`, `se_rating`, and
  `stopped_due_to_se` are predictors for `effectiveness` and `remission` but
  not vice versa. Because every predictor is fitted in its own model, this is a
  reporting choice rather than a statistical one.
- **Overdispersion.** No negative-binomial check has been run against the
  Poisson `n_se` model.
