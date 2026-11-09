# Analysis Pipeline

Reference for the structure of the analysis, its outputs, and the traps that are
easy to fall into. Written for someone (or some agent) picking the project up
cold.

## Data source: the ilovedata extracts

The analysis frames are built from the **ilovedata extraction pipeline**
(`~/Yandex.Disk/Projects/Research/pipelines/ilovedata`), not from the old
`glad-data-extraction` `.rds` files. One path builds both cohorts:

| Cohort | Surveys | Joined on |
|---|---|---|
| Baseline | `glad` (sign-up: covariates), `glad_med` (standalone medication survey), `glad_fh` (Family History) | `person_id` |
| SA8 | `glad_sa8_signup` (all three blocks in one survey) | — |

`person_id` is the same 28-character hash the old files carry as `ID`, so old
and new frames can be compared person by person.

### Step 1: export (run in the pipeline's environment)

`extract/ilovedata_export.R` reads a built store **read-only** and writes three
files. It needs the pipeline's Nix environment, and `--no-init-file` keeps this
project's renv `.Rprofile` out of that R session:

```sh
P=~/Yandex.Disk/Projects/Research/pipelines/ilovedata   # or a worktree of it
env -u ILOVEDATA_LIVE_FETCH ILOVEDATA_DUCKDB_MEMORY_LIMIT=6GB ILOVEDATA_DUCKDB_THREADS=4 \
  nix develop "$P" -c Rscript --no-init-file extract/ilovedata_export.R \
  --pipeline "$P" --store "$P/store.duckdb" --out data
```

The two `ILOVEDATA_DUCKDB_*` settings raise the engine's DuckDB budget from its
2 GB default. Under the default, the derived-variable pulls spill several
gigabytes into DuckDB's temporary directory (under `$TMPDIR`) and failed with
"No space left on device" when the disk had 2.7 GB free. With 6 GB the
1,000-per-survey store exports in about 25 minutes. `--vars <n>` (person-level
variables per call, default 6) and `--chunk <n>` (drugs per call, default 7)
can be lowered further on a full-cohort store; `ILOVEDATA_DUCKDB_TEMP_DIRECTORY`
moves the spill to a disk with room (not a cloud-synced one: it holds
participant values).

`--decisions <dir>` sets the Decision files (default
`$ILOVEDATA_DECISIONS_ROOT`, else `<pipeline>/examples/decisions`, which is where
the GLAD-Medications variables are defined today). The script refuses to run
with `ILOVEDATA_LIVE_FETCH` set and never contacts Qualtrics. Nothing in it
depends on the size of the store: the same command works on the 1,000-per-survey
development store and on a full-cohort one. It stops, rather than skipping,
if any of the 28 antidepressants' items cannot be resolved to exactly one
Anchor per survey.

| File in `data/` | Grain | Contents |
|---|---|---|
| `ilovedata_person.rds` | person | `cohort`, survey membership (`in_glad`, `in_glad_med`, `in_glad_fh`, `in_glad_sa8_signup`), every person-level variable under its pipeline name |
| `ilovedata_person_drug.rds` | person × antidepressant | taken tick, 30 side-effect ticks, `se_recorded` (checklist cells with a stored raw value, 0–30), "No side effects" tick, stopped, effectiveness, further symptoms and improvement duration per page (`_a`/`_b`), start age, duration taken |
| `ilovedata_manifest.rds` | — | snapshot, pipeline commit, store, decisions root, export time; the Anchor used for each item × drug × survey is in `attr(, "anchor_map")` |

Item columns keep their **Response Status** in the value (the pipeline's
`sentinel = TRUE`): an answer, `NA` for never shown, or -777 / -888 / -999 /
-555 / -666 for seen-not-answered / don't know / prefer not / not applicable /
implausible. `answered()`, `answer_value()` and `is_tick()` in
`lib/01.munge_helpers.r` read them; the munge decides missing versus zero from
the status and never from a blanket `na.rm`.

### Step 2: munge (ordinary `load.project()`)

`munge/01.dat_clean.r` builds `dat_long` and `dat` from the two extracts, applying
the rulings in `docs/sa8-migration-plan.md` (sections C and D) where each is
cited. (That plan is not yet committed; it lives in the main checkout.) Who
enters:

- **Antidepressant takers** are people who ticked at least one of the 28 named
  antidepressants on either list (ruling 2026-11-07). People who ticked "None",
  and people whose only tick is "a different antidepressant that isn't listed
  above", are in neither frame. Membership never comes from the
  any-side-effects question or the rating items: SA8 people who were never
  routed into the medication section currently show as "seen, not answered"
  there, because the pipeline cannot yet compile SA8's nested routing.
- `dat_long`: one row per taker × one of the 28 named antidepressants ticked.
- `dat`: one row per taker, so exactly the people in `dat_long`, with
  `n_meds` ≥ 1. The munge stops if the two frames' participants differ. (On the
  1,000-per-survey sample: `dat_long` 1,754 rows, 638 people; `dat` 638 rows.)
- Both keep only people with a depressive or anxiety disorder
  (`mhd.depression_and_anxiety` known and not "neither"), as before.
- A person who answered both sign-ups (`cohort` "Baseline+SA8") keeps their
  Baseline answers (ruling 2026-11-08), as the export already writes them, and
  their medication rows come only from `glad_med`; the munge reports the count
  and continues, but stops if any of them took a named antidepressant (3 such
  people in the full cohort, none a taker).

`cohort` (Baseline / SA8) is a column of both frames for descriptive tables.
It is **not a model term** (ruling `wave` = c).

### Contract changes relative to the old frames

- `n_relatives` is replaced by `fh_diagnosis_types`, the number of diagnosis
  types with an affected immediate relative (focal predictor; in the GLMM
  sensitivity adjustment set; in the legacy adjustment set).
- `remission` is now remission (1 = worked moderately/very well and no further
  symptoms; 0 = further symptoms, or did not work at all), not the raw
  "further symptoms" item.
- `n_se` (ruling A2, A3 as amended 2026-11-06): the number ticked; 0 for a No
  on the any-side-effects question (the checklist is not shown after No); 0 for
  Yes when the drug's checklist page was displayed and recorded with nothing
  ticked, with or without the "No side effects" tick; missing when the drug's
  checklist cells are empty (page never reached or not recorded). "Displayed
  and recorded" is `se_recorded > 0`: the export counts the drug's checklist
  cells holding a raw value (Qualtrics' 0 or -77), because the sentinel coding
  gives -77 and an empty cell the same -777. `mean_n_se` averages over known
  counts only.
- `stopped_due_to_se` (ruling 2026-11-07) follows `n_se`, because the stop
  item is shown only when a side effect was ticked: 0 for every row whose
  `n_se` is 0, whatever the reason (gate No, the "No side effects" tick, a
  recorded checklist with nothing ticked), without looking at the stop item's
  own status; missing where `n_se` is missing; otherwise the answer, and
  missing when the item was seen and not answered. This matches the old values
  for gate No and the "No side effects" tick; it is now also 0 for a recorded
  checklist with nothing ticked.
- `intolerance` (rulings 2026-11-09) enters the models as the raw proportion
  of the participant's antidepressants stopped because of side effects,
  multiplied by 10, so its odds ratio is per extra 10% of antidepressants
  stopped. It is no longer cut into three equal-width bands. The numerator is
  the drugs with `stopped_due_to_se` = 1; the **denominator is the drugs whose
  `stopped_due_to_se` is known** (not missing), not every taken drug as
  before, so a drug with an unknown status is in neither. It is missing when
  no drug has a known status. Label: "Treatment discontinuation (per 10% of
  antidepressants stopped)".
- `comorbidity_total_count_numeric` is, under ruling `comorb_groups`, an exact
  linear combination of the ten diagnosis dummies (dep/anx + bipolar/psychotic
  + ED + OCD + PD + ADHD + ASD). Both pipelines therefore fit it in its own
  model: the GLMM since 2026-08-05, the legacy per-participant block since
  2026-11-07 (bundled with the dummies, that design matrix was rank deficient by
  one column). Each legacy outcome has one more predictor set than before; the
  count keeps its label ("Number of comorbidities"), which is how the
  workbooks, plots and `src/04.manuscript_tables.r` find it.
- `ben_rating` has five levels (1–5); raw 0 is missing.
- `first_imprv`: "Not sure" on a drug's improvement-duration item arrives
  as -888 (`dont_know`) since snapshot 8. Like every non-answer, it makes that
  drug's duration missing. `first_imprv` is the duration of the first
  medication, by valid start age and then list order, for which a duration
  was reported, so after a "Not sure" it comes from a later drug. It is
  missing only if no drug has one. Settled: the owner kept this rule on
  2026-11-08.
- `comorbidity_total_count_numeric` counts the eight ruled groups.
- The 37 individual illness ticks are gone from the frames (they entered no
  model); the six `score_*` group counts remain. `illnesses` and
  `grouped_illnesses` are no longer cached.
- `dat` no longer contains anyone with `n_meds = 0`: neither glad_med
  respondents who ticked "None" nor takers of only "a different
  antidepressant".
- `time` is missing, rather than 0, for a taker who gave no duration for any
  drug.
- `time` (rulings 2026-11-09) is the **mean duration per antidepressant in
  years**: each drug's duration band is converted to months at its midpoint
  (1–3 months → 2, 4–6 → 5, 7–12 → 9.5, 1–2 years → 18, 2–3 → 30, 3–4 → 42)
  and averaged over the drugs with an answer, then divided by 12. The top
  band, "more than 4 years", is open; it is **assumed to be 5 years (60
  months)**. It was first the sum of the band codes 1–7, then (morning of
  2026-11-09) the sum of the midpoints, i.e. the total duration. Its odds
  ratio is per additional year on each antidepressant. Label: "Mean duration
  per antidepressant (years)"; `src/05.manuscript_numbers.r` reports the
  median and IQR of the per-participant mean (`mean_duration_*` keys).
- The leftover join columns of the old `dat_long` (`sample.x`, `startDate.y`,
  the per-drug improvement columns, ...) are gone.

### Switching a checkout to the new data

ProjectTemplate auto-loads **every** file in `data/` and, with
`cache_loading: TRUE`, every object in `cache/`. In the main checkout, before
the first `load.project()` on the new data:

1. **Move the 35 old `.rds` files out of `data/`** (into a sibling folder
   outside the project, for example `../GLAD-Medications-legacy-data/`). The new
   munge does not read them, but they would still be loaded on every run, and
   they are irreplaceable, so they must be moved, not deleted. Keep
   `data/.gitignore`.
2. **Move the model caches out of `cache/`**: `sef_models`,
   `sef_models_compete`, `eff_models`, `eff_models_compete`, `glmm_models`,
   `glmm_models_cov`, `glmm_models_cov_famhist` and the `cache/glmm_fits/`
   directory. `fit_or_cached()` reuses a cached legacy model object by **name**,
   so without this step `src/02.regression.r` would report old-data models as
   current. (The GLMM per-set fits are keyed by a hash of the data and would
   simply not be hit, but they are stale too.) The cached legacy objects also
   predate the 2026-11-07 split of the comorbidity count into its own model, so
   reusing them would silently keep the rank-deficient bundled set. Also move
   `illnesses.RData` and
   `grouped_illnesses.RData`, which nothing writes any more.
3. Run the export with `--out data`, then `load.project()`. The munge's
   `cache("dat")`, `cache("dat_long")` and `cache("labels")` overwrite the old
   cached frames, which is why step 2 should keep a copy of `cache/` as a whole.
4. The one refit, on full-cohort data, is then `FORCE_REFIT=TRUE Rscript
   src/02.regression.r` (see "Traps").

`results/medications_sig_final.xlsx` is not touched by any of this.

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
The legacy per-participant effectiveness models (`ben_rating`, `n_best`,
`first_imprv`) use the same arrangement: `nonlong_indeps_eff` holds the three
side-effect measures as three separate one-element sets, not one joint set.

## Adjusted models

GLMM adjustment set: `sex`, `start_age`, `cumulative_med_count`, `phq9`; the
sensitivity set adds family history (`fh_diagnosis_types`, formerly
`n_relatives`). Legacy adjustment set: `sex`, `avg_start_age`,
`fh_diagnosis_types`. These are **covariate-only** — `get_adjusted_indeps_list_for_dep()`
strips them from the focal predictor list so a variable never competes with
itself, and `drop_covariate_terms()` removes them from the output tables
(matching on prefix, so expanded factor terms like `sexFemale` are caught too).

They remain in the fitted model; they are only absent from the reported rows.

## Products

Everything below except `docs/` is **gitignored** and not in the repository.

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

### `docs/` — manuscript tables and numbers (tracked, regenerated in place)

These two scripts read only cached objects and the frames; neither fits a
model. Their outputs are tracked files that are regenerated in place and left
uncommitted after a run, like the result workbooks. Both take
`MANUSCRIPT_OUT_DIR` (default `docs`) so that a dry run, for example on the
development extracts, can write to `logs/` instead of over the tracked files.

| Script | Writes | From |
|---|---|---|
| `src/04.manuscript_tables.r` | `docs/manuscript-tables-current.md` and, in `docs/manuscript-tables/`: `table_0.html` (Table 1, sample by cohort), `table_{1a,1b,1c,2a,2b,2c,2d,2e}.html` (Tables 2a–2c and 3a–3e, significant associations), `supp_{1a,...,2e}.html` (Supplementary Tables S2a–S3e) | `dat`; `glmm_models`, `glmm_models_cov`; `sef_models`, `eff_models`, `sef_models_compete`, `eff_models_compete` |
| `src/05.manuscript_numbers.r` | `docs/manuscript-numbers-current.md`: every stand-alone number the text quotes, as key / value / definition | the extracts, `dat`, `dat_long`; `glmm_models`, `glmm_models_cov`, `glmm_models_cov_famhist` where present |

- **Table numbering.** The manuscript renumbered its tables on 2026-11-09:
  the sample table is Table 1 (was 0), the side-effect tables 2a–2c (were
  1a–1c), the effectiveness tables 3a–3e (were 2a–2e), and the supplementary
  tables S2a–S3e (were S1a–S2e). `src/04.manuscript_tables.r` writes the new
  numbers into the captions (`label` in `TABLES`); the HTML files keep their
  old names (`id`), so `table_1a.html` is Table 2a and `supp_2e.html` is
  Supplementary Table S3e.
- **Table 1** (`table_0.html`) describes `dat` by cohort and overall: sex, employment,
  relationship status, PHQ-9, GAD-7, AUDIT, BMI, comorbidity count, family
  history, number of antidepressants, as n (%) of known values or mean (SD),
  with a Missing column. Age is shown once the munge carries an `age` column
  (the pipeline's `dem.selfreport_age` is not exported yet); education is not
  in the extract.
- **Supplementary Tables S2a–S3e** are the full output behind Tables 2a–3e, one
  each, same outcome and model family: every parameter of every predictor set,
  significant or not, with the unadjusted and adjusted estimate (nominal 95%
  CI), Bonferroni-corrected p and complete-case N (observations / participants
  for the GLMM). A dash in the adjusted column marks an adjustment covariate,
  † a fit that reported a convergence warning. They are new: the manuscript
  cited them but nothing produced them.
- **The numbers file** covers the inclusion funnel, frame sizes by cohort,
  antidepressants per participant, the survey period, known-outcome counts,
  the outcome distributions per medication and per participant, the remission
  discordance, the share with no side effects from any medication, family-
  history coverage and, from the cached models, the largest model N, ICCs, the
  Pearson dispersion of the `n_se` models and the family-history sensitivity
  comparison. The export adds four `Date` columns to the person frame,
  `response_date_glad`, `response_date_glad_med`, `response_date_glad_fh` and
  `response_date_glad_sa8_signup`, read from the surveys' Embedded Data
  `<Block>_StartDate` / `<Block>_EndDate` anchors (`anchor_resolutions_as_of()`,
  `get_data(sentinel = FALSE)`; the anchors are recorded in
  `attr(manifest, "response_date_anchors")`). In the full-cohort export
  `response_date_glad_sa8_signup` is populated for every SA8 person
  (2024-04-02 to 2026-11-07) and `response_date_glad` for 17,099 of the 59,240
  Baseline people (2020-05-04 to 2025-04-07); the rest of `response_date_glad`
  is `NA`. `response_date_glad_med` and `response_date_glad_fh` are entirely
  `NA`: those surveys have no `<Block>_StartDate` / `<Block>_EndDate` fields,
  and the ilovedata store holds no other response date for them (Qualtrics
  `startDate` is dropped by the live fetch, ilovedata issue 404). The
  1,000-per-survey development sample had no Baseline dates at all, only SA8
  ones (2024-04-02 to 2024-10-21), so a Baseline survey period from
  `response_date_glad` covers only the people who have one.
- A table or number whose model objects are not in `cache/` is skipped with a
  message naming the objects, and the markdown says so; an old HTML file for
  that table is left as it was. Table 1 and the data-only numbers need no
  model.
- The dispersion is recorded on each Poisson fit by `fit_glmm()` (attribute
  `dispersion`, since 2026-11-08), because the cached objects keep no fitted
  model. Fits made before that carry none, and the numbers file says so.

### Order of work after a refit

1. `FORCE_REFIT=TRUE Rscript src/02.regression.r > logs/regression_<date>.log 2>&1`
   (see "Traps"); check the convergence report at the end of the log.
2. `Rscript src/04.manuscript_tables.r > logs/tables_<date>.log 2>&1`: Table 1,
   Tables 2a–3e, S2a–S3e.
3. `Rscript src/05.manuscript_numbers.r > logs/numbers_<date>.log 2>&1`: the
   text's numbers. Neither 2 nor 3 depends on the other.
4. `Rscript src/03.results_plot.r > logs/plots_<date>.log 2>&1`: the figures.
5. Copy the tables and numbers into the manuscript from
   `docs/manuscript-tables/*.html` and `docs/manuscript-numbers-current.md`.

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
`dplyr::mutate()` for this reason — keep them qualified. The same applies to
`munge/01.dat_clean.r`: the `src/` scripts attach tidyverse before
`reload.project()`, so a bare `rename()` there resolved to `plyr::rename` and
stopped the munge, although `load.project()` on its own ran clean.

**Label lookups need the legacy names.** The cached `labels` object does not
contain `mean_n_se`, `intolerance` or `time`. Both `src/02.regression.r` and
`src/03.results_plot.r` add them via a local `labels_extra` block. Omitting it
silently drops those predictors from grouped plots rather than erroring. The
two blocks must agree: labels are baked into the cached model objects when
they are fitted, so a label changed in one script only reaches the tables
after those model sets are refitted, and `src/03.results_plot.r` groups
predictors by matching the same text. `reports/compare_legacy_glmm_results.R`
(local only) also keys on the label text in `parameter_aliases`.

**Plot x-axis is log-scaled and uncapped.** `lib/07.plot_funcs.r` draws the
estimate axis with `scale_x_log10()` (breaks 0.1 to 4) and no `coord_cartesian`
cap, so every interval fits inside its panel. The bars are Bonferroni-widened:
`save_grouped_plots()` in `src/03.results_plot.r` passes the models through
`bonferroni_ci()` (`lib/03.multi_adjust.r`) after `multi_adjust()`, so colour
(corrected significance) and interval agree. The axis label says so.

## Open questions

These are unresolved analysis decisions, not defects.

- **Cross-outcome predictor asymmetry.** `n_se`, `se_rating`, and
  `stopped_due_to_se` are predictors for `effectiveness` and `remission` but
  not vice versa. Because every predictor is fitted in its own model, this is a
  reporting choice rather than a statistical one.
- **Overdispersion.** No negative-binomial check has been run against the
  Poisson `n_se` model. Since 2026-11-08 `fit_glmm()` records the Pearson
  dispersion of each Poisson fit and `src/05.manuscript_numbers.r` reports it;
  the full-cohort value awaits the refit.
