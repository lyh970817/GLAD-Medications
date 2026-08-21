# Improvement suggestions — GLAD antidepressants manuscript

Scope as briefed: **small changes only** — completeness and correctness. No
reframing, no new analyses beyond what is already fitted, no reorganising, no
rewriting the Introduction.

Ranked by strength gained per unit of effort. Items 1–3 are correctness problems
found while checking the current output; they are not optional polish, and two
of them would be caught by a reviewer.

---

## Tier 1 — do these; they are defects, not improvements

### 1. The adjusted GLMM Remission model has produced degenerate confidence intervals

**Effort: none to report, unknown to fix. Impact: blocks reporting.**

In `results/glmm_medications_cov.xlsx`, sheet `Remission`, ten rows carry
zero-width confidence intervals with p < 0.001:

```
Eating disorders                   0.78[0.78, 0.78]   <0.001
ADHD                               0.69[0.69, 0.69]   <0.001
Obsessive compulsive disorders     0.80[0.80, 0.80]   <0.001
Personality disorders              0.72[0.72, 0.72]   <0.001
Autism spectrum disorders          0.78[0.78, 0.78]   <0.001
Only anxiety disorder      v.s …   1.15[1.15, 1.15]   <0.001
Only depressive disorder   v.s …   1.20[1.19, 1.20]   <0.001
Only bipolar disorder      v.s …   0.61[0.61, 0.61]   <0.001
Only psychotic disorder    v.s …   1.47[1.47, 1.47]   <0.001
Psychotic and bipolar      v.s …   1.32[1.32, 1.32]   <0.001
```

Every affected row is a categorical psychiatric-diagnosis predictor. The
continuous and employment/relationship predictors in the same sheet have normal,
wide intervals (`Doing unpaid or voluntary work 0.91[0.53, 1.59]`), so this is
not a formatting or rounding artefact — the interval genuinely collapsed onto the
point estimate. That is the signature of a singular or non-converged `glmer` fit,
or of a variance–covariance matrix that could not be computed for those terms.

An OR of exactly `1.47[1.47, 1.47]` cannot be published. Note also that this is
the *unadjusted* Remission model's counterpart: `glmm_medications_sig.xlsx`
Remission is clean, with sensible intervals throughout.

**Recommendation.** Do not put the adjusted GLMM Remission column into Table 2d
until this is diagnosed. Diagnosing it needs the cached fit inspected
(`isSingular()`, the convergence warnings, the term-level `vcov`) — that is a
read of `cache/`, not a refit. Fixing it may need that one model refitted, which
is your call.

### 2. Figures and tables apply different multiple-testing corrections

**Effort: one line of code plus a replot. Impact: removes a visible contradiction.**

`multi_adjust()` in `lib/03.multi_adjust.r` multiplies p-values by
`length(models)` — the number of outcomes in whatever object it is handed. It is
called on different objects for the workbooks and for the grouped panels:

| Output | Object passed | Outcomes | Bonferroni factor |
|---|---|---|---|
| `medications*.xlsx` | `c(sef_models, eff_models)` | 4 | **× 4** |
| `glmm_medications*.xlsx` | `glmm_models` | 4 | **× 4** |
| `glmm_all*.png` | `glmm_models` | 4 | × 4 |
| `glmm_eff*` / `glmm_sef*` panels | `glmm_models_eff` / `_sef` | 2 | **× 2** |
| `eff*` panels | `eff_models` | 3 | **× 3** |
| `sef*` panels | `sef_models` | 1 | **× 1 — no correction at all** |

So a predictor can be coloured orange in a figure and be missing from the
corresponding table. This is not hypothetical; I verified two cases. In
`sef_Psychiatric_History.png`, **ADHD** (1.33 [1.01, 1.77]) and **Only depressive
disorder vs. depressive and anxiety disorder** (0.85 [0.72, 1.00]) are both drawn
in orange. Their corrected p-values in `medications.xlsx` are 0.177 and 0.192, so
neither appears in Table 1b. A reader comparing Figure 1a with Table 1b sees the
figure claim two significant associations the table denies.

**Recommendation.** Pass the same grouping to `multi_adjust()` in
`src/03.results_plot.r` that the workbooks use, then regenerate the panels from
the cached fits. No refitting is involved — `src/03.results_plot.r` reads model
objects that `reload.project()` restores.

### 3. The Methods description of the correction does not match the code

**Effort: one sentence. Impact: correctness.**

Methods currently says:

> "We first divided the models into two sets, the first set comprises those
> models examining the three side effect-related outcomes, the second comprising
> those examining treatment effectiveness outcomes and corrected the *p* values
> within each group with the Bonferroni method by the number of outcomes."

That describes × 3 and × 5. The code corrects by × 4 within each *workbook*,
which does not correspond to the two sets described — the GLMM workbook mixes
side-effect and effectiveness outcomes in one set of four, and the legacy
workbook mixes one side-effect outcome with three effectiveness outcomes. Either
the sentence or the code has to move. Changing the sentence is the cheap option
and is honest, provided item 2 is fixed so the figures agree.

---

## Tier 2 — high value, low effort

### 4. State the sample size at both levels

**Effort: one sentence plus a table note. Impact: high.**

The manuscript says 8,937 participants throughout. The GLMM tables carry N values
of 22,999 / 25,453 / 17,457 / 20,260 — those are **medication episodes**, not
people. A reader who sees "N = 25,453" in a study described as having 8,937
participants will assume an error. The legacy tables meanwhile carry
6,556–7,842, which are participants after complete-case exclusion, also
unexplained.

Say it once in Methods and once as a table footnote: how many participants
contributed, how many medication episodes, and the mean number of antidepressants
per participant. All three are already derivable from `dat_long_glmm`.

### 5. Add the limitation that follows from one model per predictor

**Effort: two sentences. Impact: high — this is the first thing a reviewer will ask.**

Every focal predictor is fitted in its own model (`indeps_list` is a list of
vectors that `fit_model()` maps over). Predictors therefore never compete for
variance, and every reported association is **marginal, not independent**. The
manuscript never says this, and the Discussion reads in places as though the
associations were mutually adjusted — for example when it contrasts
comorbidities, family history and depressive episodes as if each were a separate
contribution.

Suggested addition to *Strengths and limitations*:

> Each candidate variable was modelled separately rather than jointly, so the
> reported associations are marginal and may reflect shared variance between
> correlated predictors. The adjusted models control for sex, starting age,
> family history, cumulative medication count and depressive symptoms, but not
> for the other candidate variables.

### 6. Report the random-intercept variance or ICC

**Effort: one number per model, read from the cached fits. Impact: high for a GLMM paper.**

The paper's central methodological claim is that clustering medication episodes
within participants matters. Nothing currently quantifies how much. One line —
the participant random-intercept variance, or the ICC — makes the claim checkable
and is the first thing a methods reviewer looks for. It can be read straight off
the cached `glmm_models` objects; no refit.

### 7. Reconcile "average starting age" and "total duration" with the GLMM

**Effort: wording only. Impact: prevents a direct table/text contradiction.**

The Abstract, Discussion and Conclusion all attribute findings to **average
starting age** and **total duration on antidepressants**. Neither variable exists
in the GLMM: both are per-person summaries with no medication-episode analogue.
The GLMM uses `Start Age` (the age at which *that* medication was started) and
`Cumulative Medication Count` instead. If any GLMM estimates go into the Results,
these claims must be explicitly attributed to the legacy per-person models, or
restated in terms of the GLMM variables.

Related and worth fixing at the same time: the GLMM grouped panels currently
show **neither** pair. `predictor_groups_list` in `src/03.results_plot.r` lists
only the legacy names `avg_start_age` and `time` under `Demographics_Lifestyle`,
so the GLMM's own `Start Age` and `Cumulative Medication Count` are filtered out
and silently dropped — the same name-mismatch that dropped `mean_n_se` and
`intolerance` from the legacy panels before the 2026-07-27 fix. Both are
significant: `Start Age` for Effectiveness (1.02 [1.02, 1.02], p < 0.001),
Number of Side Effects (p = 0.010) and Stopped due to Side Effects (0.98 [0.98,
0.98], p < 0.001); `Cumulative Medication Count` for Remission (0.91 [0.89, 0.93],
p < 0.001). Adding the two GLMM names to that group and replotting fixes it.

### 8. Put numbers on the PHQ-9 sensitivity analysis

**Effort: one sentence. Impact: moderate.**

Currently: *"Including the sum scores for PHQ-9 has little impact on the results,
indicating that there is no evidence for negative reporting bias from depressive
symptoms in our sample."* "Little impact" is an assertion the reader cannot
check. Give the count — how many associations changed significance status, and
whether any changed direction. The numbers are already in
`medications_cov.xlsx` / `glmm_medications_cov.xlsx`.

---

## Tier 3 — worth doing, small gain

### 9. Handle the confidence intervals that run off the panel edge

`lib/07.plot_funcs.r` sets `coord_cartesian(xlim = c(NA, 2), clip = "off")`. Ten
intervals exceed that bound and are drawn leaving the panel with no truncation
marker; two are significant — "Only psychotic disorder" on Benefit rating
(1.58 [1.03, 2.41]) and "Retired vs. in paid employment" on First improvement
duration (1.52 [1.11, 2.08]). Cheapest honest fix is a caption sentence saying
the axis is truncated at 2 and giving the affected estimates in the table.
Raising the cap compresses every other panel, so I would not do that.

### 10. Mention that no overdispersion check was run on the Poisson count model

`n_se` is a Poisson `glmer` over a side-effect checklist. Checklist counts
commonly overdisperse, and Poisson standard errors are anticonservative when they
do — which matters because the GLMM's headline is that it finds *more*
significant side-effect associations than the legacy models. Either run a
dispersion statistic on the cached fit (cheap, no refit) or add one limitation
sentence. Doing neither leaves the headline claim's most obvious alternative
explanation unaddressed.

---

## Not worth doing

- **Rewriting the two-part-model justification in *Strengths and limitations*.**
  The paragraph defending the gamma/logistic two-part model against previous
  approaches becomes moot the moment the count outcome is fitted as a Poisson
  GLMM. Delete it rather than rewrite it — a rewritten defence of a model you no
  longer use costs effort and gains nothing.
- **Adding a colour legend to every panel.** One sentence in each caption saying
  orange = significant, grey = non-significant does the same job. Regenerating 38
  plots to add a legend is not worth it.
- **Fixing the Remission marker shape inconsistency** (triangle in `glmm_all.png`,
  square in the `glmm_eff*` family). Only visible if both figures ship, and the
  grouped panels are what is going in.
- **Reporting the legacy and GLMM estimates side by side for every outcome.** The
  comparison is genuinely interesting — 18 significant side-effect associations
  versus 1 — but presenting it properly is a second paper's worth of framing, and
  the brief rules out restructuring. If you want it, one sentence in the
  Discussion noting the direction of the difference is the whole affordable
  version.
- **Chasing the `n_relatives` / `cumulative_med_count` covariate overlap.** Both
  are in the adjustment set and both are also focal predictors elsewhere;
  `get_adjusted_indeps_list_for_dep()` already strips a variable from its own
  focal list, so there is no self-adjustment bug to fix. It reads oddly but is
  correct.
