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
It is **not a term of the main models** (ruling `wave` = c); since 2026-11-09
it is a covariate in the survey-cohort sensitivity sets only (see "Survey-cohort
sensitivity set").

### Contract changes relative to the old frames

- `n_relatives` is replaced by `fh_diagnosis_types`, the number of diagnosis
  types with an affected immediate relative (focal predictor; in the GLMM
  sensitivity adjustment set; in the legacy adjustment set).
- `remission` is now remission (1 = worked moderately/very well and no further
  symptoms; 0 = further symptoms, or did not work at all), not the raw
  "further symptoms" item.
- `n_se` (rulings A2 and A3; **strict rule, owner ruling 2026-11-12**, which
  reverses the 2026-11-06 amendment of A3): the number ticked; 0 for a No on
  the any-side-effects question (the checklist is not shown after No); 0 for
  Yes when the drug's checklist page was displayed and recorded with nothing
  ticked **and its "No side effects" box ticked**; **missing** for Yes when the
  page was displayed and recorded with nothing ticked and the "No side effects"
  box not ticked (the 2026-11-11 audit's categories "c mixed" and "c77 stored
  -77"); missing when the drug's checklist cells are empty (page never reached
  or not recorded). "Displayed and recorded" is `se_recorded > 0`: the export
  counts the drug's checklist cells holding a raw value (Qualtrics' 0 or -77),
  because the sentinel coding gives -77 and an empty cell the same -777.
  `mean_n_se` averages over known counts only. On snapshot2 the strict rule
  leaves 3,273 `dat_long` rows (Baseline 2,428, SA8 845) of 1,675 participants
  missing that were 0 under the zero rule; known `n_se` rows 28,920 (zero rule
  32,193); 62 participants have no known count (they stay in both frames:
  entry depends on the drug ticks and the diagnosis scope only).
- `n_se_zero`, `stopped_due_to_se_zero` (since 2026-11-12): the same two
  variables under the **zero rule** (A3 as amended 2026-11-06, the main rule
  until 2026-11-12), which codes those blank checklists as 0 and not stopped.
  The munge stops if they differ from `n_se` / `stopped_due_to_se` anywhere but
  on those rows. They are read only by the zero-rule sensitivity set
  (`lib/13.zero_se.r`); `dat` has no zero-rule columns.
- `stopped_due_to_se` (ruling 2026-11-07) follows `n_se`, because the stop
  item is shown only when a side effect was ticked: 0 for every row whose
  `n_se` is 0 (gate No, or the "No side effects" tick), without looking at the
  stop item's own status; missing where `n_se` is missing (so also on a blank
  checklist without the "No side effects" tick); otherwise the answer, and
  missing when the item was seen and not answered. Known on 27,436 `dat_long`
  rows (zero rule 30,709).
- `intolerance` (rulings 2026-11-09) enters the models as the raw proportion
  of the participant's antidepressants stopped because of side effects,
  multiplied by 10, so its odds ratio is per extra 10% of antidepressants
  stopped. It is no longer cut into three equal-width bands. The numerator is
  the drugs with `stopped_due_to_se` = 1; the **denominator is the drugs whose
  `stopped_due_to_se` is known** (not missing), not every taken drug as
  before, so a drug with an unknown status is in neither. It is missing when
  no drug has a known status. Label: "Treatment discontinuation (per 10% of
  antidepressants stopped)". `mean_n_se` and `intolerance` follow the main
  (strict) coding; on snapshot2 `mean_n_se` is known for 10,712 of 11,104
  participants (zero rule 10,774; mean 3.61 vs 3.36) and `intolerance` for
  10,689 (zero rule 10,763; mean 29.2% vs 26.2% of known-status drugs).
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
   directory. Until 2026-11-12 `fit_or_cached()` reused a cached legacy model
   object by **name**, so without this step `src/02.regression.r` would have
   reported old-data models as current (it now checks an input digest; see
   "Cache behaviour" under "Traps"). (The GLMM per-set fits are keyed by a hash of the data and would
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
| Model objects | `glmm_models`, `glmm_models_cov`, sensitivity sets `glmm_models_cov_famhist`, `glmm_models_cov_class`, `glmm_models_cov_cohort`, `glmm_models_cov_drug`, `glmm_models_cov_zero` | `sef_models`, `eff_models`, `sef_models_compete`, `eff_models_compete`, sensitivity sets `sef_models_cohort`, `eff_models_cohort` |
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

## Within/between decomposition (hybrid GLMMs)

Since 2026-11-09 (user ruling), every **focal predictor that varies within a
participant** in `dat_long` is split, in the same model, into two terms
(`lib/09.within_between.r`):

- `X_between`: the participant's mean of X over their medications with a
  known X;
- `X_within`: X minus that mean.

The participant random intercept is kept. The mean is taken over every row of
the frame being fitted with a known X, not over each model's complete-case
rows. The within-person term is informed only by participants with two or more
medications (73.8% of `dat_long` participants on the full cohort; reported by
`src/05.manuscript_numbers.r` as `participants_two_or_more_medications_*`).

Which predictors vary within participant was checked on the full-cohort
`dat_long`: `start_age`, `cumulative_med_count`, `n_se` and
`stopped_due_to_se`. Everything else, `se_rating` included, is constant
within a person. So the decomposed sets are:

| Set | Decomposed focal predictors |
|---|---|
| unadjusted (`glmm_models`) | `start_age`, `cumulative_med_count` (all four outcomes); `n_se`, `stopped_due_to_se` (effectiveness, remission) |
| adjusted (`glmm_models_cov`), family-history sensitivity (`glmm_models_cov_famhist`), drug-class sensitivity (`glmm_models_cov_class`), survey-cohort sensitivity (`glmm_models_cov_cohort`) | `start_age`, `cumulative_med_count` (all four outcomes, one extra set each, see below); `n_se`, `stopped_due_to_se` (effectiveness, remission) |
| drug-identity sensitivity (`glmm_models_cov_drug`) | only these four sets, for effectiveness and remission only (see "Drug-identity sensitivity set") |
| zero-rule blank-checklist sensitivity (`glmm_models_cov_zero`) | `n_se`, `stopped_due_to_se` (effectiveness, remission); `cumulative_med_count`, `start_age` (`n_se`, `stopped_due_to_se`) only (see "Zero-rule blank-checklist sensitivity set") |

`start_age` and `cumulative_med_count` are also **adjustment covariates** in
the adjusted sets. Since 2026-11-10 (user ruling) they play both roles there:

- In every ordinary focal set of an adjusted family they are covariates and
  are entered **undecomposed**, as before.
- Each of them also gets **one focal set of its own** per outcome, its within
  and between terms, adjusted for all the other covariates of that family
  (sex, PHQ-9, the other of the two, plus family history, the class dummies
  or the cohort in the sensitivity families) but **not for its own whole
  version**. `get_adjusted_indeps_list_for_dep()` keeps the single-variable set
  of a competing covariate that is in `WITHIN_BETWEEN_VARS` (appended after the
  other sets, so every existing set keeps its position), the caller decomposes
  it, and `fit_family()` gives each job its own covariates through
  `covs_for_set()` (`lib/09.within_between.r`), which drops a covariate whose
  two terms are the focal set. For every other set `covs_for_set()` returns the
  family's covariates unchanged, in the same order, so their cache keys and
  cached fits are untouched. That is 2 predictors x 4 outcomes x 4 adjusted
  families = 32 models.

`decompose_indeps_list()` runs after `get_adjusted_indeps_list_for_dep()` has
removed the other covariates from the focal list, so a decomposed two-term set
cannot slip past that filter. Note that `drop_covariate_terms()` matches on
prefix: the focal `start_age_within` would be dropped by a covariate named
`start_age`, which is a second reason the own-variable set must not carry its
whole version among its covariates.

The two terms carry the predictor's label with " (within person)" and
" (between persons)" appended, e.g. "Number of Side Effects (within person)".
`src/03.results_plot.r` puts both in the predictor's own panel group (for
start age and the cumulative medication count that is
`*_Demographics_Lifestyle`, now in the adjusted `glmm_*_cov_*` panels too, at
the bottom of the panel), and `src/04.manuscript_tables.r` shows both rows in
the supplementary tables (and whichever is significant in the Results tables),
with a footnote; in Tables 3a, 3c, 4a and 4d and S2a, S2c, S3a, S3d the
start-age and cumulative-count rows now carry an adjusted estimate, and in
S4a–S4d and S5a–S5d they appear as rows. `src/05.manuscript_numbers.r`
reports the main adjusted estimates as
`adjusted_<outcome>_{start_age,cumulative_med_count}_{within,between}`. The
legacy per-participant models are unaffected: their predictors are constant
within person by construction.

## Adjusted models

GLMM adjustment set: `sex`, `start_age`, `cumulative_med_count`, `phq9`; the
sensitivity set adds family history (`fh_diagnosis_types`, formerly
`n_relatives`). Legacy adjustment set: `sex`, `avg_start_age`,
`fh_diagnosis_types`. These are **covariate-only** — `get_adjusted_indeps_list_for_dep()`
strips them from the focal predictor list so a variable never competes with
itself — with one exception: the two GLMM covariates that vary within person
(`start_age`, `cumulative_med_count`) also have one decomposed focal set each,
adjusted for the other covariates only (see "Within/between decomposition").
So in the GLMM tables only `sex` (and, in the family-history set,
`fh_diagnosis_types`) lacks an adjusted estimate. `drop_covariate_terms()` removes the covariates from the output tables
(matching on prefix, so expanded factor terms like `sexFemale` are caught too).

They remain in the fitted model; they are only absent from the reported rows.

### Drug-class sensitivity set (`glmm_models_cov_class`)

Since 2026-11-09 (user ruling). The adjusted GLMMs (`glmm_models_cov`, with
their within/between terms) refitted on the **class-scoped** `dat_long`, with
drug-class dummies (SSRI the reference) added to the covariates. Class is
**not** in the main sets.

- The mapping is in `lib/10.drug_class.r`, copied from GLAD-SEM
  `R/drug_class.R` (commit 30daa69, GLAD-SEM ruling 2026-08-30): SSRI =
  citalopram, escitalopram, fluoxetine, fluvoxamine, paroxetine, sertraline;
  TCA = amitriptyline, clomipramine, dosulepin, doxepin, imipramine,
  lofepramine, nortriptyline, trimipramine; SNRI = duloxetine, venlafaxine;
  NaSSA = mirtazapine, mianserin; SARI = trazodone. Dropped: phenelzine,
  tranylcypromine, isocarboxazid, moclobemide, vortioxetine, reboxetine,
  tryptophan, dapoxetine and, by this project's ruling, hypericum_perforatum
  (not on the GLAD-SEM menu). It is strict: a medication name outside the 28
  is an error.
- `apply_class_scope()` removes the dropped drugs' rows (a participant left
  with none leaves) and adds `drug_class_tca`, `drug_class_snri`,
  `drug_class_nassa`, `drug_class_sari`. The within/between means are
  recomputed over the class-scoped rows.
- Every adjusted predictor set gets the four dummies as covariates (not
  reported, like the other covariates). One extra set per outcome has the four
  dummies as its **focal** set, adjusted for the usual covariates; the class
  contrasts ("Drug class: TCA vs SSRI", ...) are reported from it. So each
  outcome has one more set than in `glmm_models_cov`.
- Reported in `results/glmm_medications_cov_class.xlsx` / `_sig_cov_class.xlsx`,
  in Supplementary Tables S4a–S4d of `src/04.manuscript_tables.r`
  (`supp_class_{1a,1c,2a,2d}.html`, main adjusted vs class-adjusted, numbering
  provisional) with the class-scoped N in the footnotes, and in
  `docs/manuscript-numbers-current.md` (section "Drug-class sensitivity
  analysis": class-scoped rows and participants, and the four outcomes × four
  class contrasts).
The GLMM covariates `start_age` and `cumulative_med_count` vary within person
and are entered undecomposed, except in their own decomposed focal sets (see
"Within/between decomposition"); their within/between means are recomputed
over the class-scoped rows like the others.

### Survey-cohort sensitivity set (`glmm_models_cov_cohort`, `sef_models_cohort`, `eff_models_cohort`)

Since 2026-11-09 (user ruling: "Cohort can be included for sensitivity
analyses"). The adjusted models with `cohort` (SA8 vs Baseline; a two-level
factor coded 0/1, SA8 = 1, label "Survey cohort (SA8 vs. Baseline)") added to
the covariates. Cohort is **not** in the main sets.

- Longitudinal: `glmm_models_cov` (with its within/between terms) refitted on
  the **full** `dat_long` frame, with no row scoping, so the rows and the
  within/between means are those of the main adjusted set. `cohort` is never
  missing in the frames, so each model's N equals its main counterpart's.
- Per participant: `sef_models_compete` / `eff_models_compete` with `cohort`
  added to the covariates (`dat` frame).
- In both, one extra set per outcome has `cohort` as its **focal** set, adjusted
  for the usual covariates; the cohort contrast is reported from it, and in
  every other set the cohort is a covariate and is not reported. So each
  outcome has one more set than in the main adjusted set. (In the
  per-participant block `fit_model_non_glmm()` therefore drops only the
  covariates that are not the focal set, as `fit_set_cached()` does.)
- The longitudinal sets use the per-set cache in `cache/glmm_fits/`; the
  per-participant ones are cached by name with an input digest
  (`fit_or_cached()`) and take minutes.
- Reported in `results/glmm_medications_cov_cohort.xlsx` / `_sig_cov_cohort.xlsx`
  and `results/medications_cov_cohort.xlsx` / `_sig_cov_cohort.xlsx`; in
  Supplementary Tables S5a–S5d (`supp_cohort_{1a,1c,2a,2d}.html`, main adjusted
  vs cohort-adjusted, one per longitudinal outcome) and S5e
  (`supp_cohort_legacy.html`, the four per-participant outcomes in one table),
  numbering provisional; and in `docs/manuscript-numbers-current.md`, section
  "Survey-cohort sensitivity analysis": the cohort contrast per outcome
  (`cohort_<outcome>_sa8_vs_baseline`) and the significance changes against the
  main adjusted set.

### Drug-identity sensitivity set (`glmm_models_cov_drug`)

Since 2026-11-11 (user approval; it reports in the main Results). **Why:** the
within-person contrast of the side-effect measures is confounded by the drug.
Drugs with more side effects also tend to be rated as working better, so a
person's medication with more side effects than their own average is also,
on average, a drug that tends to work better, and that masks the
within-person association. An ad hoc check on 2026-11-10
(`logs/adhoc-class-within-2026-11-10/`) found the adjusted within-person odds
ratio of the number of side effects on effectiveness at 0.99 [0.98, 1.00]
(corrected p 0.162), 0.97 with drug-class adjustment and 0.96 [0.95, 0.97]
with the individual drug, equal to the between-person term.

- **Scope:** outcomes effectiveness and remission only, and only the focal
  sets of the four decomposed medication-level predictors (`n_se`,
  `stopped_due_to_se`, `start_age`, `cumulative_med_count`), each as its
  within and between terms in one model: 2 x 4 = 8 models.
- **Adjustment:** exactly as `glmm_models_cov` (sex, start age, cumulative
  medication count, PHQ-9, with `covs_for_set()` dropping start age or the
  cumulative count from its own set) plus `drug_identity`.
- **Frame:** the **full** `dat_long_glmm`, no row scoping, so the
  within/between terms and their means are those of the main sets (each
  model's N equals its main counterpart's).
- **Drug identity** (`lib/12.drug_identity.r`, `add_drug_identity()`): one
  factor, **sertraline the reference** (the most common drug); a drug with
  **fewer than 50 rows of the frame is pooled into "other"**. The pooled list
  is derived from the data and printed in the `src/02` log; on the full cohort
  it is doxepin (46), phenelzine (46), trimipramine (39), tranylcypromine
  (26), mianserin (17), isocarboxazid (4), dapoxetine (2), so 21 drug
  contrasts. Drug identity is a covariate only: it is never a focal set and
  its contrasts are never reported (`drop_covariate_terms()` drops the
  `drug_identity*` terms by prefix).
- **Bonferroni factor ×4** (the four longitudinal outcomes), passed
  explicitly everywhere (`multi_adjust(..., n_deps = 4)`), because the set has
  only two outcomes and the default would correct by two.
- Per-set cache in `cache/glmm_fits/`, like the others. The four ordinal fits
  have 27 fixed effects and take about 2.5 h each (in parallel); being fitted
  after 2026-11-11 they carry their thresholds and covariance (see "Clinical
  quantities").
- Reported in `results/glmm_medications_cov_drug.xlsx` /
  `_sig_cov_drug.xlsx` (one tab per outcome); in **Supplementary Table S6**
  (`supp_drug.html`, the two outcomes stacked as in S5e: main adjusted vs
  drug-adjusted estimate, corrected p, N); in
  `docs/manuscript-numbers-current.md`, section "Drug-adjusted within/between
  sensitivity analysis": `drug_<outcome>_<predictor>_<within|between>`, the
  pooled drugs (`drug_identity_pooled_drugs`) and the significance changes
  against `glmm_models_cov` (`drug_sensitivity_*`, over the 16 matched
  within/between associations); and in `docs/manuscript-numbers-clinical.md`,
  the predicted change for one side effect above the person's own mean
  (`pp_within_n_se_plus1_{adjusted,drug_adjusted}_{very_well,not_at_all}_*`,
  see "Clinical quantities").

### Zero-rule blank-checklist sensitivity set (`glmm_models_cov_zero`)

Since 2026-11-12 (owner ruling; it replaces the strict-rule set
`glmm_models_cov_strict` of 2026-11-11, ruling `a3_strict_sens` in
`docs/sa8-migration-plan.md`). **The main rule is now the strict rule** (see
`n_se` under "Contract changes"): with Yes on the any-side-effects question, a
drug whose checklist was displayed and recorded with nothing ticked counts as 0
side effects only when its "No side effects" box is ticked; otherwise its
`n_se` and `stopped_due_to_se` are missing (the 2026-11-11 audit's categories
"c mixed" and "c77 stored -77", `logs/adhoc-se-zero-2026-11-11/`). The **zero
rule**, the main rule from the 2026-11-06 amendment of A3 until 2026-11-12,
codes those rows as 0 side effects and not stopped, with or without the tick.
It is the supplementary sensitivity analysis.

- **Columns:** the munge writes `n_se_zero` and `stopped_due_to_se_zero` into
  `dat_long` beside the main columns and stops if they differ from them
  anywhere but on those rows (all missing under the main rule, all 0 under the
  zero rule). On snapshot2: 3,273 rows (Baseline 2,428, SA8 845) of 1,675
  participants; 62 participants have no known count under the main rule and
  one under the zero rule. The munge log prints the same counts before the
  diagnosis-scope filter (3,376 rows, 1,729 participants, 64).
- **Frame:** `apply_zero_se()` (`lib/13.zero_se.r`) puts the zero-rule values
  in `n_se` / `stopped_due_to_se`, drops the `_zero` columns and recomputes
  the within/between terms, so the participant means are over the drugs with
  a known zero-rule value. Same rows as `dat_long_glmm`. The recode counts come
  from `zero_se_info()` and are printed in the `src/02` log; the frame carries
  no extra attribute (see below).
- **Cache reuse:** the model frame of each zero-rule set (ID, outcome and fixed
  columns, the data part of `set_cache_path()`) is identical, column names and
  values, to the frame the same set had in `glmm_models_cov` under the
  previous main rule (code up to `a3e15bd`), so its key is the same and the
  fits made for the previous main sets are reused; `GLMM_RECIPE_VERSION` was
  not bumped. Checked on 2026-11-12 for all eight sets. This is also why
  `apply_zero_se()` attaches no attribute: a tibble's `[` keeps custom
  attributes, so an attribute on the frame enters every set's data digest.
  (The strict-rule frame of 2026-11-11 carried one, `strict_se`, so the strict
  sets fitted then do not match the new main sets' keys even though the values
  are identical.)
- **Scope:** the eight adjusted decomposed models the coding can move (as in
  the ad hoc analysis, `logs/adhoc-strict-se-2026-11-11/`): effectiveness and
  remission on `n_se` and on `stopped_due_to_se`; `n_se` and
  `stopped_due_to_se` on `cumulative_med_count` and on `start_age`. Covariates
  exactly as `glmm_models_cov`, with `covs_for_set()`. Bonferroni ×4 (passed
  explicitly). Per-set cache in `cache/glmm_fits/` like the other GLMM sets.
- Reported in `results/glmm_medications_cov_zero.xlsx` /
  `_sig_cov_zero.xlsx`; in **Supplementary Table S7** (`supp_zero.html`, which
  replaces `supp_strict.html`; caption "Within-person and between-person
  associations when blank side-effect checklists are coded as zero"; the four
  outcomes stacked: main rule vs coded as zero, corrected p, N; the footnote
  states both rules and the recoded rows as a total); and in
  `docs/manuscript-numbers-current.md`, section "Zero-coded blank-checklist
  sensitivity analysis": the recode counts (`zero_blank_checklist_*`,
  `zero_participants_no_known_n_se`, `zero_*_known_rows`), the estimates
  `zero_<outcome>_<predictor>_<within|between>` and the significance changes
  against `glmm_models_cov` (`zero_sensitivity_*`, 16 matched associations).
  These keys replace the `strict_*` keys one for one.

**Significance changes** (`src/05.manuscript_numbers.r`,
`add_significance_changes()`): for the cohort set (longitudinal and
per-participant separately) and for the drug-class set
(`class_sensitivity_*`), the associations matched by outcome and parameter
label whose Bonferroni-corrected significance (p ≤ 0.05) differs from the main
adjusted set, counted as lost, gained and total, with the associations named
(and, since 2026-11-11, for the drug-identity set, `drug_sensitivity_*`).
They are computed on the **unrounded** p values in the cached models; a count
read off the tables, which round p to three decimals, can misplace a p near
0.05. A parameter estimated in only one set (the class or cohort contrasts) is
not matched. Since 2026-11-10 the matched associations include the adjusted
within- and between-person rows of start age and the cumulative medication
count, which every adjusted family now estimates.

## Products

Everything below except `docs/` is **gitignored** and not in the repository.

### `results/` — workbooks written by `src/02.regression.r`

| File | Contents |
|---|---|
| `glmm_medications.xlsx` / `_sig.xlsx` | GLMM unadjusted, all rows / significant only |
| `glmm_medications_cov.xlsx` / `_sig_cov.xlsx` | GLMM adjusted |
| `glmm_medications_cov_famhist.xlsx` / `_sig_cov_famhist.xlsx` | GLMM adjusted, family history added (sensitivity) |
| `glmm_medications_cov_class.xlsx` / `_sig_cov_class.xlsx` | GLMM adjusted, class-scoped frame with drug class (sensitivity) |
| `glmm_medications_cov_cohort.xlsx` / `_sig_cov_cohort.xlsx` | GLMM adjusted, survey cohort added (sensitivity) |
| `glmm_medications_cov_drug.xlsx` / `_sig_cov_drug.xlsx` | GLMM within/between sets of effectiveness and remission, individual drug added (sensitivity) |
| `glmm_medications_cov_zero.xlsx` / `_sig_cov_zero.xlsx` | GLMM within/between sets with blank side-effect checklists coded as zero (sensitivity; replaces `glmm_medications_cov_strict.xlsx`) |
| `medications.xlsx` / `_sig.xlsx` | legacy unadjusted |
| `medications_cov.xlsx` / `_sig_cov.xlsx` | legacy adjusted |
| `medications_cov_cohort.xlsx` / `_sig_cov_cohort.xlsx` | legacy adjusted, survey cohort added (sensitivity) |
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

These three scripts read only cached objects and the frames; none fits a
model. Their outputs are tracked files that are regenerated in place and left
uncommitted after a run, like the result workbooks. Both take
`MANUSCRIPT_OUT_DIR` (default `docs`) so that a dry run, for example on the
development extracts, can write to `logs/` instead of over the tracked files.

| Script | Writes | From |
|---|---|---|
| `src/04.manuscript_tables.r` | `docs/manuscript-tables-current.md` and, in `docs/manuscript-tables/`: `table_0.html` (Table 1, sample characteristics, overall), `table_{1a,1b,1c,2a,2b,2c,2d,2e}.html` (Tables 3a–3c and 4a–4e, significant associations), `supp_{1a,...,2e}.html` (Supplementary Tables S2a–S3e), `supp_class_{1a,1c,2a,2d}.html` (drug-class Supplementary Tables S4a–S4d), `supp_cohort_{1a,1c,2a,2d,legacy}.html` (survey-cohort Supplementary Tables S5a–S5e), `supp_drug.html` (drug-adjusted within/between Supplementary Table S6), `supp_zero.html` (zero-rule blank-checklist Supplementary Table S7; `supp_strict.html` until 2026-11-12) | `dat`, `dat_long`; `glmm_models`, `glmm_models_cov`, `glmm_models_cov_class`, `glmm_models_cov_cohort`, `glmm_models_cov_drug`, `glmm_models_cov_zero`; `sef_models`, `eff_models`, `sef_models_compete`, `eff_models_compete`, `sef_models_cohort`, `eff_models_cohort` |
| `src/05.manuscript_numbers.r` | `docs/manuscript-numbers-current.md`: every stand-alone number the text quotes, as key / value / definition | the extracts, `dat`, `dat_long`; `glmm_models`, `glmm_models_cov`, `glmm_models_cov_famhist`, `glmm_models_cov_class`, `glmm_models_cov_cohort`, `glmm_models_cov_drug`, `glmm_models_cov_zero`, the legacy sets and `sef_models_cohort` / `eff_models_cohort` where present |
| `src/06.clinical_quantities.r` | in `docs/manuscript-tables/`: `table_cross_effectiveness_stopping.html` (Table 2) and `table_predicted_probabilities.html` (Table 5); a block between `<!-- BEGIN/END src/06.clinical_quantities.r -->` markers in `docs/manuscript-tables-current.md`; the sidecar `docs/manuscript-numbers-clinical.md` (key / value / definition) | `dat_long`; the fitted remission models recovered from `glmm_models_cov` (else `glmm_models`); the drug-adjusted effectiveness ~ number of side effects set of `glmm_models_cov_drug` (thresholds and covariance kept on the cached fit); the medication counts in `docs/manuscript-numbers-current.md`, as a check |

- **Table numbering.** The manuscript renumbered its tables on 2026-11-09:
  the sample table is Table 1 (was 0), the side-effect tables 2a–2c (were
  1a–1c), the effectiveness tables 3a–3e (were 2a–2e), and the supplementary
  tables S2a–S3e (were S1a–S2e). `src/04.manuscript_tables.r` writes the new
  numbers into the captions (`label` in `TABLES`); the HTML files keep their
  old names (`id`), so `table_1a.html` is Table 2a and `supp_2e.html` is
  Supplementary Table S3e. **Renumbered again on 2026-11-11**: the
  effectiveness-by-stopping cross-table moved forward from Table 4 to Table 2,
  so the side-effect tables are 3a–3c (were 2a–2c) and the effectiveness
  tables 4a–4e (were 3a–3e); Table 1, Table 5 and every supplementary table
  kept their numbers. `label` in `TABLES` carries the main-text number and
  `supp_label` the supplementary one (S2a–S3e), which no longer follows from
  it; `table_1a.html` is now Table 3a, `table_2a.html` Table 4a, and
  `supp_1a.html` still Supplementary Table S2a. `src/06` takes its own table
  numbers (2 and 5, and 4a/4d for the cached tables it cites) from
  `CLINICAL_TABLE_NUMBERS` in `lib/11.clinical_quantities.r`.
- **No survey cohort in main-text tables** (ruling 2026-11-11). Table 1 and
  Table 2 describe the sample overall, and no main-table caption or footnote
  names the cohorts, the sign-up versions or the data source. The cohorts
  appear only in the survey-cohort sensitivity tables S5a–S5e, whose note
  labels them as the earlier (Baseline) and later (SA8) version of the GLAD
  sign-up questionnaire. The S7 footnote gives the recoded rows as a total
  (`zero_se_recoded_text(by_cohort = FALSE)`); the markdown index and the
  logs keep the split. `src/06` still computes the by-cohort cross-table
  counts and proportions, checks them, and writes them as number keys.
- **Table 1** (`table_0.html`) describes `dat` overall (one column and a Missing column): sex, employment,
  relationship status, PHQ-9, GAD-7, AUDIT, BMI, comorbidity count, family
  history, number of antidepressants, as n (%) of known values or mean (SD),
  with a Missing column. Age is shown once the munge carries an `age` column
  (the pipeline's `dem.selfreport_age` is not exported yet); education is not
  in the extract.
- **Supplementary Tables S2a–S3e** are the full output behind Tables 3a–4e, one
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
- A table or number whose model objects are not in `cache/`, or were not
  assembled by `src/02` from the current frames (source-frame stamp, see
  "Cache behaviour" under "Traps"), is skipped with a message naming the
  objects, and the markdown says so; an old HTML file for that table is left
  as it was. Table 1 and the data-only numbers need no model.
- The dispersion is recorded on each Poisson fit by `fit_glmm()` (attribute
  `dispersion`, since 2026-11-08), because the cached objects keep no fitted
  model. Fits made before that carry none, and the numbers file says so.

### Clinical quantities (`src/06.clinical_quantities.r`)

Since 2026-11-09. Reads `dat_long` and the cached models. The only models it
fits are the three adjusted ordinal effectiveness models of Table 5 (see
below; authorised 2026-11-09 for exactly these three), kept in
`cache/clinical_fits/` and reused, so only the first run on new data fits
them. It must run **after** `src/04.manuscript_tables.r`, which rewrites
`docs/manuscript-tables-current.md` in full; this script replaces only its own
marked block there (appending it if absent), so re-running it is idempotent.
Its numbers go to a sidecar, `docs/manuscript-numbers-clinical.md`, rather than
into `src/05`'s file, because `src/05` also rewrites its file in full and works
from the cached result tables alone. Like `src/04` and `src/05` it takes
`MANUSCRIPT_OUT_DIR`. Helpers are in `lib/11.clinical_quantities.r`.

- **Table 2** (was Table 4 until 2026-11-11): effectiveness (not at all /
  moderately / very well / unknown) by stopped because of side effects (not
  stopped / stopped / unknown), per medication, for the sample overall (the
  table and its footnote have no cohort blocks; the by-cohort counts and
  proportions are number keys only); cells n (row %). Unknown keeps every
  missing value visible (effectiveness "don't know", unanswered or not shown;
  stop status unknown with the number of side effects, or the item unanswered).
  Two derived proportions, over medications with a known stop status: of those
  that worked moderately or very well, the share stopped because of side
  effects; of those that did not work at all, the share not stopped. **Their
  95% CIs are a participant cluster bootstrap** (2,000 resamples of
  participants, percentile interval), so the clustering of medications within
  people is respected. The run stops unless each block's total equals the rows
  of `dat_long` for that group and the `medication_observations_dat_long_*`
  keys of `docs/manuscript-numbers-current.md`.
- **Table 5**: predicted probabilities of each outcome category (effectiveness
  not at all / moderately / very well; remission) from the adjusted
  longitudinal models (for remission the cached `glmm_models_cov` fits, the
  unadjusted set as fallback, and the footnote says which was used), one
  model per side-effect measure: (i) the same medication
  not stopped vs stopped, the participant's mean (`stopped_due_to_se_between`)
  at its observed value, i.e. the within-person contrast; (ii)
  `n_se_between` at its 25th vs 75th percentile over the model's participants,
  `n_se_within` at 0; (iii) `se_rating` at its 25th vs 75th percentile over
  participants (an observed rating; the models code it 1-6, displayed 0-5).
  **Predictions average over the model's own complete-case rows with the
  random intercept set to 0** (a typical participant: conditional, not
  marginal over the random effect, so they sit below the observed marginal
  rates), and **95% CIs are a parametric bootstrap of the fixed effects** (1,000
  multivariate normal draws from `vcov()`), with differences in percentage
  points from the same draws. The run stops if a contrast does not have the
  sign of its cached odds ratio (Tables 4a/4d): for effectiveness, the top
  category moves with the log odds ratio and the bottom one against it.
- **Where the fitted models come from.** `fit_glmm()` keeps no fitted model, but
  for the lme4 fits one survives: `parameters()` stores the call, whose
  evaluated formula carries the environment of `fit_glmm()` with `m`, `data`,
  `fit_data` and `scales` in it (which is why each cached glmer set is ~18 MB).
  `cached_fit_frame()` recovers it. The `clmm` call names `fm` as a bare
  symbol, so the cached ordinal (effectiveness) sets keep only the
  exponentiated focal estimates and CIs: no thresholds, no covariate
  coefficients, no covariance. So the three adjusted effectiveness sets
  (stopped within/between, number of side effects within/between, severity
  rating) are **refitted** by `fit_clinical_clmms()`: the formula is the cached
  set's own `model_formula`; the frame is built as `src/02` builds
  `dat_long_glmm` (`coerce_glmm_predictor()`, now in `lib/02.fit_funs.r` and
  shared by both scripts, then `add_within_between()`), with `fit_glmm()`'s
  complete cases and no scaling (as for every clmm). The run stops unless the
  rebuilt rows and participants equal the cached set's N, and unless the
  refitted odds ratios and CIs equal Table 4a's to two decimals. Each full
  `clmm` object is saved with its rows in `cache/clinical_fits/<key>.rds`, the
  key a hash of formula and data, and reused while both are unchanged. The
  three fits run on `CLINICAL_WORKERS` (default 3) PSOCK workers; fit times are
  in `docs/manuscript-numbers-clinical.md` (`clinical_fit_minutes_*`).
- **One side effect above the person's own mean** (since 2026-11-11; number
  keys only, no table rows): from the adjusted effectiveness ~ number of side
  effects model, the change in P(very well) and P(not at all) when
  `n_se_within` goes from 0 to +1, `n_se_between` and the covariates observed,
  random intercept 0, averaged over the model's rows, with a 1,000-draw
  parametric bootstrap CI; for the main adjusted model (the refit above) and
  for the drug-adjusted one from `glmm_models_cov_drug`, which is **not
  refitted**: since 2026-11-11 `fit_glmm()` keeps an ordinal fit's thresholds,
  coefficients (covariates included) and their covariance on the result
  (attributes `clmm_alpha`, `clmm_beta`, `clmm_vcov`, carried through
  `format_model()`), and `cached_clmm_predictor()` predicts from them on the
  set's complete-case rows rebuilt by `clmm_job()` (N checked against the
  cached set; the drug factor expanded by `model.matrix()`). Ordinal sets
  cached before that date carry no such attributes, which is why the three
  Table 5 models are still refitted.
  Keys `pp_within_n_se_plus1_{adjusted,drug_adjusted}_{very_well,not_at_all}_{at_0,at_plus1,difference}`.
  This block draws after Table 5 under its own seed, so Table 5 is unchanged.

### Order of work after a refit

1. `FORCE_REFIT=TRUE Rscript src/02.regression.r > logs/regression_<date>.log 2>&1`
   (see "Traps"); check the convergence report at the end of the log.
2. `Rscript src/04.manuscript_tables.r > logs/tables_<date>.log 2>&1`: Table 1,
   Tables 3a–4e, S2a–S7.
3. `Rscript src/05.manuscript_numbers.r > logs/numbers_<date>.log 2>&1`: the
   text's numbers. Neither 2 nor 3 depends on the other.
4. `Rscript src/06.clinical_quantities.r > logs/clinical_<date>.log 2>&1`, after
   2 (it writes into the file 2 rewrites) and after 3 (it checks Table 2's
   margins against 3's medication counts): Tables 2 and 5 and
   `docs/manuscript-numbers-clinical.md`. A few minutes once
   `cache/clinical_fits/` holds the three effectiveness fits; after a refit
   that changes their data, the first run refits them (20-50 minutes each,
   in parallel).
5. `Rscript src/03.results_plot.r > logs/plots_<date>.log 2>&1`: the figures.
6. Copy the tables and numbers into the manuscript from
   `docs/manuscript-tables/*.html`, `docs/manuscript-numbers-current.md` and
   `docs/manuscript-numbers-clinical.md`.

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
per-participant fitting block: because `reload.project()` restores cached
objects (the config sets `cache_loading: TRUE`), already-fitted models are
reused and only missing ones are fitted. Set `FORCE_REFIT=TRUE` to override.

Expect `Reusing cached model object: ...` in the log. If you see `Fitting: ...`
for all six, the cache is cold or the inputs changed.

**Cache behaviour** (audited 2026-11-12, when the main side-effect coding
changed). Every cache and what invalidates it:

| Cache | Key | After a data change |
|---|---|---|
| `cache/glmm_fits/<key>.rds` (every GLMM set, `fit_set_cached()` in `src/02`) | digest of `GLMM_RECIPE_VERSION`, outcome, fixed terms, dropped covariates and `data[c("ID", dep, fixed)]` (attributes of the frame included) | a set whose model frame changed gets a new key and is refitted; an unchanged one is reused |
| `sef_models`, `eff_models`, `*_compete`, `*_cohort` (`fit_or_cached()` in `src/02`, ProjectTemplate `cache()`) | by name, **plus** attribute `input_digest`: `LEGACY_RECIPE_VERSION`, outcomes, predictor sets, covariates and the frame's columns those fits read (since 2026-11-12) | refitted when the digest differs or is absent (objects cached before 2026-11-12 have none, so the first run refits all six, minutes) |
| `glmm_models*` and the legacy families as read by `src/03`-`06` (ProjectTemplate `cache()`) | by name, **plus** attributes `source_frame` / `source_digest`: the digest of `dat_long` (GLMM) or `dat` (legacy) at the end of the `src/02` run (`stamp_source_frame()`, `lib/14.model_cache.r`, since 2026-11-12) | `objects_present()` treats a family that is unstamped or stamped from a different frame as absent (`src/04`, `src/05` skip it, `src/06` stops), `src/03` stops (`stop_unless_current()`); re-running `src/02` restamps |
| `cache/clinical_fits/<key>.rds` (`src/06`, `clmm_job()` in `lib/11`) | digest of recipe, formula and the complete-case model frame | refitted when the frame changes |
| `dat`, `dat_long`, `labels`, `lab_grouped_illnesses`, `nort_recode_summary` (munge `cache()`) | none needed: `munging: TRUE` rebuilds them on every `load.project()` | always current |

So after the 2026-11-12 coding change: `src/02` refits every GLMM set whose
model frame contains `n_se` or `stopped_due_to_se` (or their within/between
terms) as outcome or focal term, in every family, and reuses the others. Checked
by computing every `set_cache_path()` key under the old (`a3e15bd`) and the new
main rule: 40 of 78 sets change in the unadjusted, class and cohort families,
38 of 74 in the adjusted one, 36 of 70 in the family-history one and 4 of 8 in
the drug one; every other key is identical. The zero-rule sets hit the previous
main sets' fits (8 of 8 keys identical). `src/02` also refits the six
per-participant families once and restamps everything; until it has run,
`src/03`-`06` refuse the cached families. A cached `glmm_models_cov_strict`
(nothing writes it any more) is dropped from the session by `src/02` and can be
deleted from `cache/`.

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
