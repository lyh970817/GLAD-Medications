# Proposed prose changes — GLAD antidepressants manuscript

**Nothing in here has been applied to the document.** Task 1 (figures and tables)
is mechanical and proceeds separately; this file is the approval gate for prose.

Working copy: <https://docs.google.com/document/d/1HLqTwR0Kpuq4ZxMEPAbeLrxd4_MN9DcLSnSPvjZ5ZJs/edit>
Original (untouched): `1cfsu36WVseKARucjVfz15kIZYw0LSOfP8iMGaIydyMc`

## How to read this

Each change gives the **exact** current text (long enough to be a unique
find-and-replace target) and the proposed replacement. I have kept your voice:
first-person plural, British spelling, the same hedging ("may", "tend to",
"indicating that"), and the same sentence lengths. Where only a number changed I
changed only the number.

**The family decision is settled: the combination.** Longitudinal estimates for
the four outcomes the GLMM covers (number of side effects, stopping because of
side effects, effectiveness, remission), per-participant estimates for the four
it does not (side effect severity rating, benefit rating, number of best
aspects, first improvement duration). Everything below assumes that.

## What has already been applied to the document

These were mechanical and are done — figures and tables, per the brief:

- All four figures replaced with the thematic grouped panels: 14 panels total,
  every one verified rendering at the full 6.27 in column width.
- The stray test image after reference 63 removed.
- All four captions rewritten: the "red" claim replaced with orange/grey, and
  the a/b split relabelled from unadjusted/adjusted to longitudinal/
  per-participant, which is what the panels now show.
- **All eight Results tables** replaced with the current numbers, each carrying
  an N footnote that states whether N counts medication observations or
  participants. Table 2d omits the adjusted column, with a footnote saying why.
- Table headings updated where the outcome changed: 1a → "the number of side
  effects", 1c → "stopping a medication because of side effects", 2a →
  "effectiveness". The two-part-model note under Table 1a was replaced.

Verified from the DOCX export: 8 tables with row counts matching the generated
tables exactly, 14 images all at full column width, no residual heading styles
inside the tables, and no stray text.

The one item from section A still outstanding is **A1**, the stray `G` in
"GResults" — left alone because it sits with the prose changes below.

---

# A. Corrections that hold under any choice

These are wrong as the document stands, independent of which estimates go in.

## A1. Stray character in the Results heading

**Before**

> \# **GResults**

**After**

> \# **Results**

## A2. Figure captions say red; every current figure is orange  ✅ APPLIED

All four captions carry the same sentence. Applied to each of Figures 1a, 1b, 2a,
2b.

**Before** (Figure 1a; the other three are identical apart from the figure number
and title)

> **Figure 1a.** **Correlates of Side Effects and Treatment Discontinuation. **Significant results are highlighted in red.

**After**

> **Figure 1a.** **Correlates of Side Effects and Treatment Discontinuation. **Significant associations after multiple testing correction are shown in orange, non-significant associations in grey.

I checked the rendered panels: significance is encoded orange against grey
throughout, with no red anywhere. This also partly answers Thalia Eley's comment
on Figure 1a.

## A3. The Methods misdescribe the multiple testing correction

`multi_adjust()` multiplies p-values by the number of outcomes in the object it
is passed, which is four per workbook — not three and five within two separate
sets. The sentence as written does not describe what was run.

**Before**

> We first divided the models into two sets, the first set comprises those models examining the three side effect-related outcomes, the second comprising those examining treatment effectiveness outcomes and corrected the *p* values within each group with the Bonferroni method by the number of outcomes [\[51\]](https://paperpile.com/c/wZ2BXg/KYhc).

**After**

> We corrected the *p* values with the Bonferroni method by the number of outcomes modelled, applied separately within the longitudinal and the per-participant analyses [\[51\]](https://paperpile.com/c/wZ2BXg/KYhc).

⚠️ **This wording is only honest once the figures are corrected.** The grouped
panels currently apply a different correction factor from the tables — the
side-effect severity panels apply none at all — so two associations (ADHD, and
only depressive disorder vs. depressive and anxiety disorder) are drawn in orange
but absent from Table 1b. See `docs/manuscript-improvements.md` item 2. If you
would rather not touch the plotting code, tell me and I will word this
differently.

## A4. The broken list numbering in the side effect analysis plan

The list runs 1) 2) 4) 5) 6) — there is no 3).

**Before**

> including 1) amount of body fat: BMI, 2) substance use: AUDIT score and smoking, 4) social factors: employment and marital status, 5) comorbidity: psychiatric and physical diagnoses and 6) intake history: total duration on antidepressants.

**After**

> including 1) amount of body fat: BMI, 2) substance use: AUDIT score and smoking, 3) social factors: employment and marital status, 4) comorbidity: psychiatric and physical diagnoses and 5) intake history: total duration on antidepressants.

## A5. In-text bold

Thalia Eley: *"don't use bold in the text."* This is a formatting pass, not a
wording change — remove bold from running prose throughout, keeping it on
headings, table headers and the Author-contributions role labels. Roughly 30
spans, concentrated in the Discussion. No words change.

One of them is a mid-word bold run worth noting, because removing the bold also
fixes the spacing:

**Before**

> we found that **the number of comorbid psychiatric diagnoses** w**as positively associated with

**After**

> we found that the number of comorbid psychiatric diagnoses was positively associated with

I will apply this only if you approve the section; it touches a lot of the
Discussion, though never the words themselves.

---

# B. Methods — describing the GLMM

Required by the brief, and needed under either the GLMM-only or the combination
option. The current *Statistical analyses* section describes only the two-part
gamma/logistic model and the partial proportional odds model, neither of which is
what produced the current longitudinal estimates.

## B1. Replace the "Continuous dependent variable" subsection

**Before**

> **Continuous dependent variable**
> As the mean number of side effects was positively skewed with clumping at zero, we used two-part regression models. A two-part regression model assumes two different data generating processes. The first process is the dichotomous event for whether the outcome occurs or not. The second process determines the exact value of the outcome, conditioned on it having occurred. Such models are estimated by separately maximising the likelihood functions of the two different classes of models. We fitted a logistic regression model for whether the outcome had occurred (i.e. has any positive value) and, after exclusion of the clumped zeroes, a gamma regression with log link for outcome values larger than zero, as the mean number of side effects remained positively skewed.

**After**

> **Longitudinal models**
> Participants reported separately on each antidepressant they had taken, so the medication is the unit of observation and observations are nested within participants. We therefore fitted generalised linear mixed models with a random intercept for participant, which allows each participant their own baseline propensity to report side effects or to rate a medication as effective, and prevents the several medications belonging to one participant from being treated as independent observations. We fitted the number of side effects as a Poisson model, so that its coefficients are rate ratios, and whether the participant stopped a medication because of side effects, and whether they experienced remission, as binomial models. Effectiveness was rated on three levels and was fitted as a cumulative link mixed model.

## B2. Replace the "Ordinal dependent variable" subsection

**Before**

> **Ordinal dependent variable**
> As all other dependent variables are ordinal with ≥ three levels, we used a partial proportional odds model. The partial proportional odds model can be formulated as *n*-1 logistic regression models, where *n* is the number of levels in the outcome variable. In case of only one explanatory variable, each logistic regression model has the same* $\beta $* representing the increased odds of being in the levels equal or less than the level represented in the model, per unit increase in the explanatory variable. 

**After**

> **Per-participant models**
> The side effect severity rating, the benefit rating, the number of best aspects and the first improvement duration are single summary responses per participant rather than per medication, so there is no within-participant variation for a longitudinal model to use. These four outcomes were fitted per participant. As all four are ordinal with ≥ three levels, we used a partial proportional odds model. The partial proportional odds model can be formulated as *n*-1 logistic regression models, where *n* is the number of levels in the outcome variable. In case of only one explanatory variable, each logistic regression model has the same* $\beta $* representing the increased odds of being in the levels equal or less than the level represented in the model, per unit increase in the explanatory variable. 

I kept your existing explanation of the partial proportional odds model verbatim
and only added the sentences saying which outcomes it now applies to and why.

## B3. Add a subsection stating that each predictor is modelled separately

Insert immediately after B1/B2, before **Multiple testing adjustment**. This is
new text, but it describes something the analysis has always done and never
stated, and it pre-empts the most likely reviewer question.

**After** (new)

> **One model per candidate variable**
> Each candidate variable was fitted in its own model rather than entering a single joint model, so that variables never compete for variance with one another. The reported associations are therefore marginal rather than mutually adjusted.

## B4. Correct the adjustment set

The Methods say the adjusted models control for three covariates. The
longitudinal models control for five, including PHQ-9; the per-participant models
control for the three named.

**Before**

> Each was examined in a separate univariate model and subsequently adjusted for sex, average antidepressant starting age and number of relatives with a psychiatric disorder.

**After**

> Each was examined in a separate univariate model and subsequently adjusted for sex, antidepressant starting age and number of relatives with a psychiatric disorder; the longitudinal models were additionally adjusted for the cumulative number of antidepressants previously taken and for PHQ-9 score.

The same sentence appears in the effectiveness analysis plan and takes the same
change.

## B5. The sensitivity analysis as described no longer exists

⚠️ **Flagging this rather than silently rewording it.** The Methods describe
PHQ-9 as a sensitivity analysis added to models that were otherwise unadjusted,
and the Results report its outcome and cite Supplementary Tables 3a–3c and 4a–4e.
In the current code PHQ-9 is a covariate *inside* the adjusted longitudinal
models, and the per-participant models do not include it at all. There is no
separate set of PHQ-9 models, so those supplementary tables cannot be
regenerated as described, and the claim in the Results has no current supporting
output.

Two honest options. **Option 1** — describe what was actually run:

**Before** (Methods, *Sensitivity analyses*)

> As a sensitivity analysis, we included the sum score as a covariate in all previous models to adjust for potential negative recall bias. 

**After**

> We included the sum score as a covariate in the adjusted longitudinal models to adjust for potential negative recall bias, so that the adjusted longitudinal estimates are already adjusted for depressive symptom load at assessment.

**Before** (Results, *Sensitivity Analyses*)

> Including the sum scores for PHQ-9 has little impact on the results, indicating that there is no evidence for negative reporting bias from depressive symptoms in our sample. Full statistics are provided in **Supplementary Tables 3a-3c** and **4a-4e**.

**After**

> The adjusted longitudinal models include the PHQ-9 sum score as a covariate, so the adjusted estimates reported above already account for depressive symptom load at assessment.

**Option 2** — keep the sensitivity analysis as a distinct result. That needs the
PHQ-9-only models fitted, which is new model fitting. I have not done it and will
not without you saying so.

I would take Option 1.

## B6. Add the limitation implied by the design

Append to *Strengths and limitations*, after the existing paragraph ending
"…we used averages across self-reported effectiveness and side effect severity
ratings."

**After** (new)

> Each candidate variable was modelled separately rather than jointly, so the associations we report are marginal and may partly reflect shared variance between correlated variables. Our adjusted models control for sex, starting age, family history, cumulative medication count and depressive symptoms, but not for the remaining candidate variables.

## B7. Delete the defence of the two-part model

This paragraph in *Strengths and limitations* argues for the two-part
gamma/logistic model against previous approaches. Once the count outcome is
fitted as a Poisson mixed model the argument no longer applies to what was run.

**Before** (final sentence of that paragraph)

> To retain the most information, we adopted a two-part zero inflated model with a gamma generalised linear model so that we could separately model whether an individual experiences side effects and the number of side effects they experience. 

**After**

> To retain the most information, we modelled the number of side effects reported for each individual antidepressant as a count, rather than dichotomising it or averaging it across a participant's medications, and allowed each participant their own baseline propensity to report side effects. 

The rest of the paragraph, which reviews how previous studies modelled side
effects, is unchanged and still reads correctly.

---

# C. Results and Discussion — number-level changes

Written against the confirmed combination decision. Still awaiting your approval
before anything here goes into the document.

## C1. Abstract — Results

Two changes: the ordering point Thalia Eley raised, and "average starting age"
and "total duration on antidepressants", which do not exist in the longitudinal
models and must not be claimed from them.

**Before**

> **Results: **Sex, average starting age, psychiatric family history, employment, relationship status, psychiatric diagnoses and comorbidities, physical conditions, number of depressive episodes, and total duration on antidepressants were associated with antidepressant side effects and effectiveness, while side effects and effectiveness were correlated with each other.

**After**

> **Results: **Side effects and effectiveness were associated with each other: a more severe side effect rating was associated with higher effectiveness, whereas a greater number of side effects, and stopping a medication because of side effects, were associated with lower effectiveness. Sex, starting age, psychiatric family history, employment, relationship status, psychiatric diagnoses and comorbidities, physical conditions, number of depressive episodes, and duration of antidepressant treatment were also associated with antidepressant side effects and effectiveness.

## C2. Discussion — split the overloaded opening clause

Thalia Eley: *"this covers quite a lot and I'm struggling to unpack it."*

**Before**

> These findings largely replicated previous results of factors statistically significantly associated with side effects and treatment effectiveness, but we also reported novel associations between self-reported number of side effects and side effect severity and antidepressant treatment effectiveness, indicating the need of reframing side effects in informed consent regarding antidepressant treatment.

**After**

> These findings largely replicated previous results of factors statistically significantly associated with side effects and treatment effectiveness. We also found that participants' experience of side effects was associated with how effective they rated the same medication. A greater number of side effects, and stopping a medication because of side effects, were associated with lower effectiveness, whereas a more severe side effect rating was associated with higher effectiveness. If patients experience side effects as a sign that a medication is having an effect, the way side effects are presented in informed consent for antidepressant treatment may need reframing.

## C3. Discussion — the treatment discontinuation effect size

The current sentence quotes the per-participant range 0.39–0.68. The longitudinal
estimate is much stronger — OR 0.08 [0.07, 0.09] for effectiveness and 0.27
[0.25, 0.30] for remission — and it is the more informative number, because it
compares different medications within the same participant.

**Before**

> **Treatment discontinuation** was associated with lower effectiveness with relatively large effect sizes (0.39-0.68) which could indicate that ineffectiveness has a significant contribution to treatment discontinuation in our sample.

**After**

> Treatment discontinuation was associated with lower effectiveness with a large effect size (OR 0.08 [0.07, 0.09] for effectiveness and 0.27 [0.25, 0.30] for remission), which could indicate that ineffectiveness has a substantial contribution to treatment discontinuation in our sample. As this association is estimated within participants, comparing the different antidepressants a person took, it is unlikely to be explained by stable differences between participants in how they report.

I have kept your hedging ("could indicate") and changed "significant" to
"substantial" only because the sentence is about effect size rather than
statistical significance, and the two sit awkwardly together.

## C4. Discussion — average starting age

**Before**

> Previous studies [\[52,7\]](https://paperpile.com/c/wZ2BXg/oS0c) have shown that individuals with a longer intake duration of antidepressant experience fewer side effects. This is consistent with our findings that **average starting age** is negatively associated with all three side effect measures. 

**After**

> Previous studies [\[52,7\]](https://paperpile.com/c/wZ2BXg/oS0c) have shown that individuals with a longer intake duration of antidepressant experience fewer side effects. This is consistent with our findings that a later starting age is associated with a lower likelihood of stopping a medication because of side effects (OR 0.98 [0.98, 0.98] per year), and with a lower side effect severity rating (OR 0.91 [0.87, 0.94] per ten years).

"All three side effect measures" is no longer accurate: in the longitudinal
models starting age is associated with stopping due to side effects and, weakly,
with the number of side effects (OR 1.00 [1.00, 1.00], p = 0.010 — significant
but null to two decimal places, which I would not report as a finding).

## C5. Discussion — the sex association

**Before**

> In terms of demographics, **being female** is associated with higher mean number of side effects and higher side effect severity rating.

**After**

> In terms of demographics, being female is associated with a higher number of side effects (RR 1.20 [1.12, 1.29]), a higher likelihood of stopping a medication because of side effects (OR 1.26 [1.11, 1.42]), and a higher side effect severity rating (OR 1.21 [1.10, 1.34]).

## C6. Conclusion — remove the variables the models no longer contain

**Before**

> For antidepressant side effects, they include the following psychiatric history variables: psychiatric diagnoses and comorbidities, number of depressive episodes, average starting age of antidepressants, total duration on antidepressants and psychiatric family history and patients’ characteristics: female sex, in employment, married marital status and increased alcohol use.

**After**

> For antidepressant side effects, they include the following psychiatric history variables: psychiatric diagnoses and comorbidities, number of depressive episodes, the age at which a medication was started, the cumulative number of antidepressants previously taken, and psychiatric family history; and patients’ characteristics: female sex, in employment, married marital status and increased alcohol use.

## C7. Sample size

⚠️ I have not drafted this because I do not have the participant count behind the
longitudinal models. The tables will carry N values of 17,457–25,453, which are
medication observations, in a paper that says 8,937 participants throughout. That
will read as an error unless stated. Getting the number means loading
`dat_long_glmm` — a data read, not a refit. Say the word and I will add:

> These *N* participants contributed *M* medication episodes, a mean of *k*
> antidepressants each.

---

# Still to come, once you have chosen

- Every remaining Discussion sentence in *Replications* that quotes a direction
  or a count of measures ("two out of three of the side effect measures", "three
  out of the five effectiveness measures"). These all change, and the arithmetic
  depends on which estimates the tables carry.
- The *Novel findings* paragraph's claim that the severity rating was associated
  with higher effectiveness "across the five effectiveness measures".
- The `Characteristics associated with…` analysis plan paragraphs, which list the
  outcome sets.
