# Comment audit — GLAD antidepressants manuscript

Audited against the original document `1cfsu36WVseKARucjVfz15kIZYw0LSOfP8iMGaIydyMc`
as read on **2026-08-05**. Nothing was resolved or replied to in Google Docs.

**11 threads.** 10 `OPEN`, 1 `RESOLVED`. One of the open threads is praise with no
action attached, so **9 are actionable** — which matches the count in the brief.

Anchor→thread mapping was recovered by matching each `<comment_start id=kix.…>`
span against thread content; every one matched unambiguously, and the comment
timestamps run in document order, which corroborates the mapping.

Verdict key: **Addressed** / **Partly addressed** / **Not addressed**.

---

## 1. Christopher Hübel — "The abstract is very clear"

- **Anchor:** the `Abstract` heading. **Status:** `OPEN`.
- **Verdict:** no action required. This is praise, not a request.
- **Fix:** none. Leave open or resolve at your discretion.

---

## 2. Thalia Eley — ordering of the abstract Results sentence

> "to me this logically comes first, with the associations with other variables
> coming after, but it depends what order you presented them in your results
> section."

- **Anchor:** the closing clause of the abstract Results: *"…while side effects
  and effectiveness were correlated with each other."*
- **Verdict: Not addressed** (text unchanged), **but the condition she attached
  is satisfied.** Her caveat was "it depends what order you presented them in
  your results section". The Results section does run side effects first
  (`Side effects and treatment discontinuation`, Tables 1a–1c) and effectiveness
  second (`Treatment effectiveness`, Tables 2a–2e), with the side-effect →
  effectiveness associations appearing inside the effectiveness tables — i.e.
  last. So the abstract's order mirrors the Results order as written.
- **The counter-argument is stronger, though.** The paper's own Discussion files
  this association under **Novel findings**, and the Conclusion ends on it:
  *"Moreover, we reported novel associations between side effect measures and
  treatment effectiveness."* Burying the novel result in a subordinate `while…`
  clause understates it — and this is the same weakness Eley raises in thread 7.
- **Proposed fix** — split the clause out and lead the Results sentence with it:

  > **Results:** Side effects and effectiveness were associated with each other:
  > participants who rated their side effects as more severe also rated the same
  > medication as more effective, whereas a greater number of side effects and
  > stopping because of side effects were associated with lower effectiveness.
  > Sex, starting age, psychiatric family history, employment, relationship
  > status, psychiatric diagnoses and comorbidities, physical conditions, number
  > of depressive episodes, and duration of antidepressant treatment were also
  > associated with side effects and effectiveness.

---

## 3. Thalia Eley — state what is *not* known about side effects

> "by the end of this paragraph I'm thinking we actually already know quite a lot
> about side effects so perhaps this is a good moment to remind the reader what
> we do NOT yet know."

- **Anchor:** the last sentence of Introduction paragraph 2 — *"Conversely, older
  individuals and individuals with long medication histories tend to report fewer
  side effects [7,8]."*
- **Verdict: Not addressed.** The paragraph still ends on that finding and the
  next paragraph switches straight to effectiveness. There is no gap statement
  anywhere in the Introduction.
- **Proposed fix** — append one sentence to that paragraph:

  > What remains unclear is whether these associations hold *within* an
  > individual across the several antidepressants most patients try, or whether
  > they reflect stable between-person differences in who reports side effects at
  > all.

  This is worth saying because it is exactly what the GLMM adds, so it sets up
  the contribution rather than just naming a gap.

---

## 4. Thalia Eley — why define effectiveness as response/non-response *(RESOLVED)*

> "I'm unclear why you are telling me this. Presumably it is also sometimes
> defined quantitatively? Are you making the point that this is the most common
> way it is defined (in which case perhaps say that)."

- **Anchor:** *"Effectiveness of antidepressants is often defined as response or
  non-response."* **Status:** `RESOLVED` (your reply is empty).
- **Verdict: Not addressed — resolved without a text change.** The sentence in
  the current manuscript is verbatim what she commented on. If this was resolved
  in the belief that it had been edited, it should be reopened; it is the same
  sentence thread 5 depends on.
- **Proposed fix:** see thread 5 — one edit closes both.

---

## 5. Thalia Eley — "poor response" is introduced without being defined

> "so far you talked about response and non-response, here you introduce poor
> response (so more quantitative). In which case this should be included as one of
> the ways to consider effectiveness in that first sentence I commented on."

- **Anchor:** *"Physical comorbidities … are also associated with **poor
  response**"*.
- **Verdict: Not addressed.** Both the defining sentence and the later "poor
  response" usage are unchanged, and the paper goes on to use at least four
  registers — "response/non-response", "poor response", "good response", and its
  own graded outcomes.
- **Proposed fix** — rewrite the defining sentence so it licenses all later uses,
  which also answers thread 4:

  > Effectiveness of antidepressants is most commonly defined dichotomously, as
  > response or non-response, but is also reported on a graded scale as poor,
  > partial, or good response. We use the graded form throughout, since GLAD
  > participants rated each antidepressant on a three-point scale.

---

## 6. Thalia Eley — the effectiveness and machine-learning literature is already good

> "again by this point I'm thinking there is already a pretty good literature on
> this question so perhaps you could make clearer what remains unknown. This isn't
> clear in the next paragraph on machine learning studies either."

- **Anchor:** the end of the clinical-profile paragraph — *"…is associated with a
  good treatment response [12,39]."*
- **Verdict: Not addressed.** That paragraph still ends on a finding, and the
  machine-learning paragraph still ends on *"…combinations of those factors
  [44,46]."* Neither says what is missing.
- **Proposed fix** — append one sentence to the machine-learning paragraph, so a
  single edit covers both paragraphs she names:

  > What these studies share is a between-person design: each participant
  > contributes one treatment outcome, so it is not known how far the identified
  > variables predict which of a patient's *own* successive antidepressants will
  > work.

---

## 7. Thalia Eley — the stated contribution sounds marginal

> "this sounds quite a marginal contribution to the literature - which is fine -
> but if you can identify anything more novel, that you are contributing to the
> literature it would help strengthen the paper. For example you mention new ones
> - can you be clearer what factors have not yet been explored (as associates of
> ADM outcomes) that are included in this study?"

- **Anchor:** *"The aim of our investigation was to replicate known associations
  and identify new ones to paint a more detailed picture of potential predictors
  of antidepressant effectiveness and side effects."*
- **Verdict: Not addressed.** The aim sentence is unchanged and still says
  "replicate known associations and identify new ones" without naming one.
- **This is the most answerable of the open comments now**, because the analysis
  has since acquired two things it did not have when she commented: a
  within-person longitudinal design, and specific predictors that are genuinely
  under-studied for antidepressant outcomes.
- **Proposed fix** — replace the aim sentence with:

  > Because participants reported on each antidepressant they had taken
  > separately, we modelled medication episodes nested within participants rather
  > than one summary per person. This let us ask which variables distinguish
  > individuals who report side effects from those who do not, and which
  > distinguish one antidepressant from another *within* the same individual — a
  > separation that between-person designs cannot make. Alongside replicating
  > established associations, we examined variables not previously reported as
  > associates of antidepressant outcomes, including autism spectrum disorder,
  > oncological and musculoskeletal conditions, alcohol use measured by AUDIT
  > score, and the cumulative number of antidepressants previously tried.

  Every variable named there is in the current output; `Autism spectrum
  disorders`, `Oncological disorders`, `Musculoskeletal & pain disorders`,
  `Alcohol use disorder` and `Cumulative Medication Count` all reach significance
  for at least one GLMM outcome.

---

## 8. Thalia Eley — Methods repeats Introduction justifications

> "you have reviewed the literature in the introduction so I'm not sure you need
> these justifications here."

- **Anchor:** the Methods *Demographics* paragraph — *"We included sex as studies
  have found differing antidepressant efficacy between males and females [10]. We
  included employment and relationship status, as social support and employment
  are associated with antidepressant effectiveness [11]. Furthermore, we included
  years of smoking … showed associations with poor response in previous studies."*
- **Verdict: Not addressed.** Unchanged, and the same pattern recurs in three
  neighbouring paragraphs she did not separately mark: *Psychiatric diagnoses*
  ("Psychiatric diagnoses have been shown to be associated with…[7,8]"),
  *Physical conditions* ("Physical conditions have been shown to be associated
  with…[16,21–23]") and *AUDIT Score* ("Alcohol use has been associated with poor
  antidepressant response [20,27]").
- **Proposed fix** — reduce *Demographics* to the measure itself, and delete the
  trailing justification sentence from the other three:

  > *Demographics*
  > We included sex, employment status, relationship status, years of smoking
  > expressed as pack years, and BMI, all as reported at sign-up to GLAD.

---

## 9. Thalia Eley — figures illegible, avoid red *(+ reply about tables)*

> "please can you redo this so that the text is MUCH larger. I'd also suggest
> avoiding red as the main colour used. Even when I scroll in the text is so
> blurry I can barely read it."
>
> reply: "If this figures simply duplicates the info in the tables I'd get rid of
> the tables but the figures do need to be legible."

- **Anchor:** the Figure 1a caption.
- **Verdict: Partly addressed — and the remainder is what this session fixes.**

  **Legibility — resolved by the new panels.** The four current figures have
  aspect ratios 2.33–2.53, above the ~1.42 point at which Docs stops fitting to
  the 6.27 in column and starts fitting to page height instead. They therefore
  render at 4.01 in × 9.35 in — 36 % of the column width thrown away, with the
  text shrunk to match. That is the mechanical cause of "blurry … I can barely
  read it", and no amount of zooming fixes it. The thematic grouped panels sit at
  ratio 0.29–1.04, so they render at the full 6.27 in. I rendered several at true
  page scale and read them: axis labels, predictor names and the legend are all
  comfortably legible.

  **Colour — already fixed in the plots, not in the captions.** Every current
  panel encodes significance in **orange**, with non-significant estimates in
  **grey**. Red is gone. But all four captions still read *"Significant results
  are highlighted in red."* — so the document currently contradicts its own
  figures. Fixing that is in Task 1.

  **The reply about duplication — Not addressed.** Tables 1a–1c and 2a–2e and
  Figures 1a/1b/2a/2b do report the same estimates; the tables list only the
  significant rows while the figures show every predictor with its CI. That is a
  real difference and worth keeping both, but the paper never says so.
- **Proposed caption fix** (applies to all four captions):

  > Significant associations after multiple-testing correction are shown in
  > orange; non-significant associations in grey. Tables 1a–1c and 2a–2e list the
  > significant associations with exact estimates; the figures additionally show
  > the non-significant associations and their confidence intervals.

- **One thing to decide:** ten confidence intervals in the current workbooks
  exceed the hard-coded x-axis limit of 2 in `lib/07.plot_funcs.r` and are drawn
  running off the panel edge with no truncation marker. Two of them are
  significant. A reviewer who notices a CI leaving the panel will ask.

---

## 10. Thalia Eley — Discussion opening clause is overloaded

> "this covers quite a lot and I'm struggling to unpack it. Perhaps make this a
> sentence (or even two) on its own and help the reader out a bit more with what
> the logical steps are you are making here."

- **Anchor:** *"…but we also reported novel associations between self-reported
  number of side effects and side effect severity and antidepressant treatment
  effectiveness, indicating the need of reframing side effects in informed consent
  regarding antidepressant treatment."*
- **Verdict: Not addressed.** Still a single trailing clause that carries a
  result, its direction, and a clinical recommendation at once. The phrase "number
  of side effects and side effect severity and … effectiveness" is also
  genuinely ambiguous to parse.
- **Proposed fix** — split into three sentences, one logical step each:

  > We also found that participants' experience of side effects was associated
  > with how effective they rated the same medication. A greater number of side
  > effects, and stopping a medication because of side effects, were associated
  > with lower effectiveness; a more severe side effect *rating*, however, was
  > associated with higher effectiveness. If patients experience side effects as a
  > sign that a drug is doing something, then the way side effects are presented
  > when antidepressant treatment is discussed may need reframing.

---

## 11. Thalia Eley — "don't use bold in the text"

- **Anchor:** Discussion, *"**the number of comorbid psychiatric diagnoses**"*.
- **Verdict: Not addressed, and the scope is much wider than the anchor.** Bold
  is used as an emphasis device throughout the Discussion — at least 30 spans,
  e.g. *"**autoimmune & inflammatory diseases, neurological conditions** and
  **respiratory & atopic conditions**"*, *"**Number of first degree relatives with
  psychiatric diagnoses**"*, *"**being female**"*, *"**Being a student**"*,
  *"**average starting age**"*, *"**Treatment discontinuation**"* — and also in
  Methods (*"**Supplementary Material**"*, *"**Supplementary Methods**"*) and in
  the Results lead-ins (*"**Supplementary Table S2a-S2e**"*).
- **Proposed fix:** remove in-text bold everywhere except headings, table headers
  and the Author-contributions role labels. This is mechanical and safe — no
  wording changes, and it also removes the odd mid-word bold at *"the number of
  comorbid psychiatric diagnoses** w**as"*, where the bold run ends one character
  into the next word.
- Worth doing in the same pass: the Results heading currently reads
  **"GResults"** — a stray leading `G`.

---

## Summary

| # | Author | Topic | Verdict |
|---|---|---|---|
| 1 | Hübel | abstract is clear | no action |
| 2 | Eley | abstract Results ordering | Not addressed (condition satisfied; still worth changing) |
| 3 | Eley | state the side-effect gap | Not addressed |
| 4 | Eley | effectiveness definition *(resolved)* | Not addressed — resolved without an edit |
| 5 | Eley | "poor response" undefined | Not addressed |
| 6 | Eley | state the effectiveness / ML gap | Not addressed |
| 7 | Eley | contribution sounds marginal | Not addressed — now much easier to answer |
| 8 | Eley | Methods repeats Intro justifications | Not addressed |
| 9 | Eley | figures illegible, avoid red | **Partly addressed** — legibility and colour fixed in the plots; captions still say "red"; duplication point open |
| 10 | Eley | Discussion clause overloaded | Not addressed |
| 11 | Eley | don't use bold | Not addressed |
