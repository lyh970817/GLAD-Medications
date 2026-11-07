# Project Status

Snapshot of where the analysis stands, so a new session does not have to
reconstruct it from transcripts. Update this when the state changes.

**As of 2026-07-27.**

## Migration to the ilovedata extracts (branch `ilovedata-migration`, 2026-11-06)

The frames are now built from the ilovedata pipeline for two cohorts, Baseline
and SA8 (see "Data source" in `docs/analysis-pipeline.md`). Verified on the
1,000-per-survey development store only: `load.project()` builds both frames,
and every GLMM and legacy design matrix builds. **No model has been refitted.**
The cached fits and products in the main checkout still describe the old data.

Resolved 2026-11-07 (owner's rulings): `stopped_due_to_se` is 0 whenever
`n_se` is 0; `dat` holds only people with a named antidepressant, exactly those
in `dat_long`; the legacy per-participant block fits the comorbidity count in
its own model (it was rank deficient bundled with the diagnosis dummies).

Open before a full-cohort refit:

- `first_imprv` when the earliest drug's improvement duration is "Not sure"
  (see the 2026-11-07 check below): today the munge takes the next drug with a
  reported duration. Needs a ruling if that should be missing instead.
- In `glad_med`, 217 "further symptoms" answers (186 people) are 0 in the old
  extraction and 1 in the pipeline, as are 62 improvement durations (old 0, new
  1–5). Effectiveness agrees on every row, so this is not a row mismatch.
  Unexplained; the raw exports need checking.

Re-export check 2026-11-07 (development store, snapshot 8, pipeline
`glad-medications-variables` at 62281792): the export and `load.project()` run
clean, with no munge change. `dat_long` 1,754 rows / 638 people and `dat` 638
(Baseline 132, SA8 506), the same people as on snapshot 7. Every outcome
distribution is unchanged. The two items that were pending in the pipeline are
now filled: SA8 AUDIT goes from 106 missing to 0 (never-drinkers scored 0), and
the eating-disorder group goes from 48 missing to 0 and the comorbidity count
from 49 to 1 (SA8 fallback). No other variable changed for anyone. "Not sure"
on the improvement-duration item now arrives as -888 (`dont_know`), not -777:
203 drug rows in `dat_long` (Baseline 41, SA8 162; 143 people). `answered()`
treats it as missing, never as a duration. 79 people have only "Not sure"
answers, so their `first_imprv` is missing. For 25 people the earliest drug
was "Not sure", so `first_imprv` comes from a later drug, as it did when
these cells were -777. In this worktree `load.project()` still needs
`RENV_CONFIG_AUTOLOADER_ENABLED=FALSE R_LIBS=<main checkout>/renv/library/linux-nixos-zokor/R-4.5/x86_64-pc-linux-gnu`,
because its own renv library holds only `renv`.

## Where things stand

The pipeline is complete and current. The longitudinal GLMM analysis was
implemented in commit `97579c4`, then refined over several sessions in February
2026. The models were last fitted on **2026-02-11 02:47**, which postdates the
last change to the fitting code — so the cached fits in `cache/` are valid for
the code as it stands, and there is no reason to refit.

All result workbooks, all 38 plots, and the legacy-vs-GLMM comparison report
were regenerated on 2026-07-27 from those cached fits. Every outcome sheet is
populated; a completeness check over the four workbooks found no empty or
partial sheets.

## What was done on 2026-07-27

- Added `fit_or_cached()` to `src/02.regression.r` so the script reuses cached
  model objects instead of unconditionally refitting. Previously a re-run cost
  ~2h20m even with a warm cache, which is what made an earlier session appear
  to hang. `FORCE_REFIT=TRUE` forces a full refit.
- Fixed a plotting bug that dropped two of the three side-effect predictors
  from the legacy `eff_Side_Effects` panels. The cached `labels` object lacks
  `mean_n_se` and `intolerance`, and the `Side_Effects` predictor group listed
  only the GLMM names. The omitted rows included Treatment discontinuation
  (OR 0.53), so the figure understated the side-effect story. Fixed in
  `src/03.results_plot.r` and verified visually.
- Regenerated all products and deleted stale ones (48 obsolete plots from
  Nov 2025 – Feb 2026, a dead LibreOffice lock file, and a truncated run log).

## What is still open

**Interpretive write-up.** `reports/legacy_glmm_comparison.md` contains the
numbers but no narrative. The headline result is that the GLMM finds far more
significant side-effect associations than the legacy aggregate models — 18
versus 1 in the adjusted comparison. Nobody has written up why.

The mechanism is understood: the participant random intercept stops treating
each participant's ~2.3 medication rows as independent observations, so
person-level predictors (sex, BMI, employment, comorbidities) get honest, wider
standard errors, while predictors that vary within a person across medication
episodes benefit from within-person contrasts. Significant remission predictors
drop from 15 to 9 for this reason — that is the model working correctly, not
signal being lost.

**`reports/results.md` is stale.** Dated 2026-02-03, pre-GLMM. It is
hand-written narrative describing a superseded analysis and needs rewriting
rather than regenerating.

**Analysis decisions.** See "Open questions" in `docs/analysis-pipeline.md` —
the `remission` model family, the cross-outcome predictor asymmetry, and an
overdispersion check for the Poisson `n_se` model.

**Plot presentation.** Ten confidence intervals exceed the hard-coded x-axis
cap of 2 in `lib/07.plot_funcs.r` and are drawn running off the panel edge
without a truncation marker. Two are statistically significant: "Only psychotic
disorder" on Benefit rating, 1.58 [1.03, 2.41], and "Retired vs. in paid
employment" on First improvement duration, 1.52 [1.11, 2.08]. Options are to
raise the cap, add arrowheads, or leave it.

Two smaller presentation inconsistencies, both cosmetic: plots encode
significance with orange versus grey but no legend explains the colour, and the
Remission marker is a triangle in `glmm_all.png` but a square in the
`glmm_eff*` family.

## Notable interpretive finding

Treatment discontinuation is strongly associated with the drug not having
worked — OR 0.08 for effectiveness in the GLMM, sharpened from 0.39 in the
legacy aggregate model. The questionnaire item is specifically "did side
effects make you stop", so "it worked but I stopped anyway" is an available
answer, and its near-absence is informative rather than tautological. The
reading is that respondents conflate side effects and effectiveness for
antidepressants, unlike chemotherapy where "effective but intolerable" is a
well-recognised category. The within-person design strengthens this: it is the
same participant giving different answers across their own medications.

## Unrelated loose end

The Zapier MCP setup (`docs/zapier-mcp-setup.md`, `.envrc.local.example`) is
half-configured — `.envrc.local` was never created with a real token. It turned
out unnecessary since `readxl` covered the need. Keep or remove at will; it has
no bearing on the analysis.
