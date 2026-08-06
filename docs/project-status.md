# Project Status

Snapshot of where the analysis stands, so a new session does not have to
reconstruct it from transcripts. Update this when the state changes.

**As of 2026-07-27.**

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
