# Next-session prompt — apply the improvements, then redo tables, figures and prose

Paste everything below the line into a fresh session started in
`/home/andongni/Downloads/GLAD-Medications`.

---

Apply the improvements we agreed on to the GLAD antidepressants manuscript, then
regenerate everything downstream of them and apply the resulting prose changes.

## Where things stand

The previous session left the working copy in a consistent, verified state:

- **Working copy:** <https://docs.google.com/document/d/1HLqTwR0Kpuq4ZxMEPAbeLrxd4_MN9DcLSnSPvjZ5ZJs/edit>
- **Original, untouched:** `1cfsu36WVseKARucjVfz15kIZYw0LSOfP8iMGaIydyMc` — never edit it.
- Branch `worktree-glmm-manuscript-update`, draft PR
  <https://github.com/lyh970817/GLAD-Medications/pull/1>.

Already done and verified from the DOCX export:

- All four figures replaced with 14 thematic grouped panels, every one rendering
  at the full 6.27 in column width. Captions rewritten (orange/grey, not red;
  a/b now means longitudinal/per-participant).
- The stray test image after reference 63 removed.
- All eight Results tables replaced with current numbers under the
  **combination** decision — longitudinal GLMM estimates for number of side
  effects, stopping because of side effects, effectiveness and remission;
  per-participant estimates for side effect severity rating, benefit rating,
  number of best aspects and first improvement duration. Each carries an N
  footnote stating whether N counts medication observations or participants.
- `lib/03.multi_adjust.r` and `src/03.results_plot.r` fixed so figures and
  tables apply the same Bonferroni factor, and so the GLMM panels no longer drop
  `Start Age` and `Cumulative Medication Count`.

**No prose has been applied.** `docs/manuscript-text-changes.md` holds the
proposed diff, approved in principle but deliberately not yet applied, because
the improvements below will change parts of it.

Read these first — they are the whole brief:

- `docs/manuscript-improvements.md` — the ranked suggestions to apply.
- `docs/manuscript-text-changes.md` — the prose diff to revise and then apply.
- `docs/manuscript-comment-audit.md` — the reviewer comments, for context.
- `docs/manuscript-tables-current.md` — how the current tables were generated.
- `docs/analysis-pipeline.md` — pipeline structure. Treat its claims as
  unverified; one was wrong last session and was corrected.

## Ground rules

**Do not refit models without asking me first.** A full refit is ~2h20m.
`src/02.regression.r` has a `fit_or_cached()` guard; `FORCE_REFIT=TRUE` only on
my explicit say-so. Reading cached fits, and re-running `src/03.results_plot.r`,
are both fine and need no permission.

**Verify claims against the code and data, not against the docs in `docs/`.**
Last session a false claim about remission being ordinal propagated from
`docs/analysis-pipeline.md` into the proposed Methods text. If a doc asserts
something load-bearing, check it.

**Browser work.** Read `.claude/skills/google-docs-editing/SKILL.md`. Two
hard-won details not yet in it: table paste fails silently once the Docs
renderer has been up a while — reload the page and it works; and a table pasted
next to a heading inherits Heading 4, turning every cell into an outline entry,
so select the pasted table and press Ctrl+Alt+0 (select top-down — bottom-up
selects one column). Preconditions: `browser-use-chrome` running and signed into
Google. If it is not signed in, stop and tell me.

## Task 1 — Decide the improvement set with me

`docs/manuscript-improvements.md` ranks ten suggestions across three tiers plus
a "not worth doing" list. Before doing anything, put the list to me and get my
call on each. Group them by what they cost:

- **Prose-only** (items 3, 5, 7, 8, 9, 10 in part) — no analysis needed.
- **Read the cached fits** (items 1 diagnosis, 4, 6) — no refit.
- **Needs a refit** (item 1 fix, item 10 overdispersion check if I want the
  negative-binomial comparison rather than just a dispersion statistic).

Recommend which to take. Do not start the refit-requiring ones until I say so.

## Task 2 — The adjusted remission model

This is the one real defect. Ten rows in `results/glmm_medications_cov.xlsx`
sheet `Remission` have zero-width confidence intervals with p < 0.001 — all of
them categorical psychiatric-diagnosis predictors. Table 2d currently omits the
adjusted column entirely and says so in a footnote.

Diagnose it from the cached fit first: `isSingular()`, the convergence
warnings, the term-level `vcov`, and the cell counts for those predictors
against remission. Complete separation is the obvious candidate. Tell me what
you find and what fixing it would cost before fitting anything.

If it is fixed, Table 2d gains an adjusted column and its footnote changes.

## Task 3 — Sample sizes at both levels

Item 4. The tables carry N values of 17,457–25,453 for the longitudinal outcomes
and 6,556–7,842 for the per-participant ones, in a paper that says 8,937
participants throughout. Get the real numbers from `dat_long_glmm`: participants
contributing, medication episodes, mean antidepressants per participant. This is
a data read, no refit. It changes the Methods, and possibly the table footnotes.

## Task 4 — Random-intercept variance

Item 6. Read the participant random-intercept variance, or the ICC, off each
cached GLMM fit. One number per model. It goes in Methods or Results and it is
the first thing a methods reviewer will look for.

## Task 5 — Regenerate whatever the above changed

If Task 2 changes the remission model, or Task 3/4 add reported quantities:

- Regenerate the affected workbooks and plots. `src/03.results_plot.r` runs from
  cached fits; expect ~15 minutes and 38 plots. The grouped panels finish well
  before the large monolithic ones, which the manuscript does not use.
- Rebuild the affected tables. The generator is
  `docs/manuscript-tables-current.md`'s companion script — regenerate the HTML
  in `docs/manuscript-tables/` the same way, keeping the N-footnote convention.
- Replace the affected tables and figures in the working copy, and verify from
  the DOCX export: table row counts, image count and order, full column width,
  no residual heading styles, no stray text.

Only touch what actually changed. Do not re-paste tables whose numbers are
unaffected.

## Task 6 — Revise the prose diff, then apply it

Rewrite `docs/manuscript-text-changes.md` so it reflects the post-improvement
state. Specifically:

- **B5** currently offers two options for the PHQ-9 sensitivity analysis. The
  Methods describe PHQ-9 as a separate sensitivity analysis with its own
  supplementary tables, but in the current code it is a covariate inside the
  adjusted longitudinal models and absent from the per-participant models
  entirely, so those tables cannot be regenerated as described. I chose option
  1 — describe what was actually run. Fold that in.
- **A3** (multiple-testing wording) can now be stated plainly, since the figure
  and table correction factors were reconciled.
- **C7** was a placeholder awaiting the participant counts from Task 3. Fill it.
- Add whatever Tasks 2 and 4 introduce: the remission model's status, and the
  random-intercept variance sentence.
- Item 5's limitation about one-model-per-predictor, and item 7's reconciliation
  of "average starting age" and "total duration" with the GLMM, both still
  apply — keep them.
- **A1**, the stray `G` in the "GResults" heading, is still outstanding.

**Preserve my writing style.** First-person plural, British spelling, the same
hedging. Do not restructure paragraphs, do not tighten prose you were not asked
to change, do not introduce new claims. Where only a number changed, change only
the number.

Show me the revised diff and wait for my approval. Then apply it to the working
copy — and only then. Bold removal (A5) touches a lot of the Discussion but
changes no words; apply it in the same pass.

## Task 7 — Re-audit the comments

`docs/manuscript-comment-audit.md` judged all 11 threads against the pre-change
text. Several of the proposed fixes will have been applied by the end of this
session. Re-check each and tell me which are now genuinely addressed, quoting
the new text. Do not resolve or reply to anything in Google Docs.

## Verification

Finish with a visual round: screenshot every changed region and confirm each
figure is legible at page scale and each table reads correctly. Confirm from the
DOCX export that image count, document order, table row counts and heading
styles are what you intended.

Tell me plainly what you changed, what you left alone, and anything you could
not verify. If something is blocked, finish everything else and say what you
left out and why.
