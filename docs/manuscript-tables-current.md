# Results tables — current numbers

Generated 2026-08-05 from the current workbooks in `results/`, under the
**combination** decision: longitudinal (GLMM) estimates for the four outcomes
the GLMM covers, per-participant (legacy) estimates for the four it does not.

| Table | Outcome | Source |
|---|---|---|
| 1a | Number of side effects | `glmm_medications_sig*.xlsx` → `Number of Side Effects` (Poisson, rate ratios) |
| 1b | Side effect severity rating | `medications_sig*.xlsx` → `Side effect severity rating` |
| 1c | Stopped because of side effects | `glmm_medications_sig*.xlsx` → `Stopped due to Side Effects` |
| 2a | Effectiveness | `glmm_medications_sig*.xlsx` → `Effectiveness` |
| 2b | Benefit rating | `medications_sig*.xlsx` → `Benefit rating` |
| 2c | Number of best aspects | `medications_sig*.xlsx` → `Number of best aspects` |
| 2d | Occurrence of remission | `glmm_medications_sig*.xlsx` → `Remission` |
| 2e | First improvement duration | `medications_sig*.xlsx` → `First improvement duration` |

Adjusted estimates appear in brackets where the adjusted model is also
significant, matching the existing manuscript convention. Rows shown only in
brackets are significant after adjustment but not before.

**Status in the document:** only **Table 1a** has been applied. See
`docs/manuscript-text-changes.md` for why the remaining seven are still
outstanding. Ready-to-paste HTML for all eight is in
`docs/manuscript-tables/` — opening one and copying it in the browser, then
pasting over the old table in the Doc, is the manual route.

**Table 2d carries a warning.** The adjusted GLMM remission model produced ten
zero-width confidence intervals, so no adjusted column is reported for that
outcome. See `docs/manuscript-improvements.md` item 1.

---


#### Table 1a: Variables significantly associated with the number of side effects
N = 25453
| Parameter | RR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.20[1.12, 1.29] | <0.001 |
| Start Age | 1.00[1.00, 1.00] | 0.010 |
| Number of relatives with psychiatric disorders | 1.02[1.02, 1.03] | <0.001 |
| Alcohol use disorder | 1.01[1.00, 1.01] (1.01[1.00, 1.01]) | 0.008 (0.031) |
| Pack years of cigarettes smoked | 0.99[0.99, 1.00] (0.99[0.99, 0.99]) | <0.001 (<0.001) |
| Full or part-time student v.s In paid employment or self-employed | 1.29[1.18, 1.41] (1.19[1.07, 1.34]) | <0.001 (0.007) |
| Retired v.s In paid employment | 0.54[0.48, 0.61] (0.53[0.45, 0.62]) | <0.001 (<0.001) |
| Unable to work because of sickness or disability v.s In paid employment or self-employed | 1.20[1.10, 1.30] | <0.001 |
| In relationship v.s Not in relationship | 1.21[1.13, 1.30] (1.18[1.09, 1.29]) | <0.001 (<0.001) |
| Married v.s Not in relationship | 0.88[0.82, 0.94] | 0.001 |
| Eating disorders | 0.83[0.73, 0.93] (0.80[0.69, 0.93]) | 0.009 (0.015) |
| Personality disorders | 0.83[0.73, 0.94] | 0.019 |
| Number of comorbidities | 1.30[1.20, 1.41] (1.23[1.11, 1.36]) | <0.001 (<0.001) |
| Number of depressive disorder episodes | 1.03[1.02, 1.04] | <0.001 |
| Cardiometabolic & endocrine Disorders | 0.87[0.83, 0.90] (0.84[0.80, 0.88]) | <0.001 (<0.001) |
| Autoimmune & inflammatory Diseases | 1.13[1.07, 1.18] | <0.001 |
| Respiratory & atopic conditions | 1.07[1.04, 1.09] | <0.001 |
| Oncological disorders | 0.70[0.56, 0.88] (0.71[0.54, 0.93]) | 0.009 (0.046) |
| BMI (kg/m^2) | (0.99[0.99, 0.99]) | (<0.001) |


#### Table 1b: Variables significantly associated with side effect severity rating
N = 7803
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.21[1.10, 1.34] | <0.001 |
| Average starting age/10 | 0.91[0.87, 0.94] | <0.001 |
| Pack years of cigarettes smoked | 0.99[0.99, 1.00] | 0.010 |
| Total duration on antidepressants | 1.02[1.01, 1.02] (1.02[1.01, 1.03]) | <0.001 (<0.001) |
| Cardiometabolic & endocrine Disorders | 0.91[0.87, 0.96] | 0.004 |


#### Table 1c: Variables significantly associated with stopping a medication because of side effects
N = 20260
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.26[1.11, 1.42] | <0.001 |
| Start Age | 0.98[0.98, 0.98] | <0.001 |
| Number of relatives with psychiatric disorders | 1.04[1.03, 1.05] | <0.001 |
| Retired v.s In paid employment | 0.69[0.56, 0.84] | <0.001 |
| Unable to work because of sickness or disability v.s In paid employment or self-employed | 1.64[1.44, 1.88] (1.37[1.14, 1.63]) | <0.001 (0.003) |
| Married v.s Not in relationship | 0.84[0.74, 0.94] | 0.011 |
| Eating disorders | 0.65[0.53, 0.79] (0.63[0.48, 0.82]) | <0.001 (0.002) |
| Obsessive compulsive disorders | 0.70[0.57, 0.85] (0.68[0.53, 0.88]) | 0.001 (0.015) |
| Number of comorbidities | 1.72[1.51, 1.96] (1.60[1.35, 1.90]) | <0.001 (<0.001) |
| Psychotic and bipolar disorder v.s No psychotic or bipolar disorder | 0.49[0.31, 0.78] (0.36[0.20, 0.67]) | 0.010 (0.005) |
| Number of depressive disorder episodes | 1.09[1.07, 1.10] (1.07[1.05, 1.09]) | <0.001 (<0.001) |
| Cardiometabolic & endocrine Disorders | 0.92[0.86, 0.98] | 0.035 |
| Neurological conditions | 1.31[1.19, 1.45] (1.24[1.10, 1.41]) | <0.001 (0.002) |
| Autoimmune & inflammatory Diseases | 1.16[1.07, 1.25] | 0.001 |
| Respiratory & atopic conditions | 1.14[1.10, 1.19] (1.12[1.06, 1.18]) | <0.001 (<0.001) |
| Oncological disorders | 0.60[0.41, 0.87] | 0.032 |
| Autism spectrum disorders | (0.61[0.42, 0.89]) | (0.039) |
| Musculoskeletal & pain disorders | (1.24[1.09, 1.41]) | (0.004) |


#### Table 2a: Variables significantly associated with effectiveness
N = 22999
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.15[1.07, 1.24] | 0.001 |
| Start Age | 1.02[1.02, 1.02] | <0.001 |
| Pack years of cigarettes smoked | 1.00[0.99, 1.00] | 0.032 |
| Doing unpaid or voluntary work v.s. In paid employment or self-employed | 0.64[0.51, 0.80] (0.55[0.41, 0.75]) | <0.001 (<0.001) |
| Full or part-time student v.s In paid employment or self-employed | 0.80[0.72, 0.88] | <0.001 |
| Unable to work because of sickness or disability v.s In paid employment or self-employed | 0.56[0.51, 0.61] (0.74[0.66, 0.82]) | <0.001 (<0.001) |
| Unemployed v.s. In paid employment or self-employed | 0.69[0.59, 0.82] | <0.001 |
| In relationship v.s Not in relationship | 1.13[1.05, 1.22] | 0.005 |
| Married v.s Not in relationship | 1.40[1.31, 1.51] | <0.001 |
| Personality disorders | 0.80[0.70, 0.91] | 0.002 |
| Number of comorbidities | 0.82[0.75, 0.89] | <0.001 |
| Only anxiety disorder v.s Depressive and anxiety disorder | 1.30[1.08, 1.57] | 0.025 |
| Only bipolar disorder v.s No psychotic or bipolar disorder | 0.68[0.59, 0.78] (0.65[0.54, 0.78]) | <0.001 (<0.001) |
| Number of depressive disorder episodes | 0.93[0.92, 0.94] (0.96[0.95, 0.97]) | <0.001 (<0.001) |
| Neurological conditions | 0.92[0.87, 0.98] | 0.028 |
| Respiratory & atopic conditions | 0.96[0.94, 0.99] | 0.007 |
| Number of Side Effects | 0.96[0.96, 0.97] (0.98[0.97, 0.99]) | <0.001 (<0.001) |
| Side effect severity rating | 1.08[1.05, 1.10] (1.09[1.06, 1.12]) | <0.001 (<0.001) |
| Stopped due to Side Effects | 0.08[0.07, 0.09] (0.08[0.07, 0.09]) | <0.001 (<0.001) |
| BMI (kg/m^2) | (1.01[1.00, 1.01]) | (0.013) |
| Retired v.s In paid employment | (0.80[0.67, 0.94]) | (0.034) |
| Musculoskeletal & pain disorders | (0.87[0.80, 0.94]) | (0.002) |


#### Table 2b: Variables significantly associated with benefit rating
N = 7741
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.56[1.41, 1.73] | <0.001 |
| Average starting age/10 | 1.09[1.05, 1.13] | <0.001 |
| BMI (kg/m^2) | 1.02[1.01, 1.02] (1.02[1.01, 1.03]) | <0.001 (<0.001) |
| Alcohol use disorder | 0.99[0.98, 1.00] | 0.005 |
| Doing unpaid or voluntary work v.s. In paid employment or self-employed | 0.55[0.40, 0.76] (0.38[0.24, 0.60]) | 0.001 (<0.001) |
| Full or part-time student v.s In paid employment or self-employed | 0.69[0.61, 0.78] (0.74[0.61, 0.89]) | <0.001 (0.008) |
| Unable to work because of sickness or disability v.s In paid employment or self-employed | 0.54[0.48, 0.61] (0.52[0.44, 0.62]) | <0.001 (<0.001) |
| Unemployed v.s. In paid employment or self-employed | 0.52[0.42, 0.65] (0.50[0.37, 0.68]) | <0.001 (<0.001) |
| In relationship v.s Not in relationship | 1.16[1.05, 1.28] | 0.018 |
| Married v.s Not in relationship | 1.54[1.39, 1.70] (1.54[1.33, 1.78]) | <0.001 (<0.001) |
| Obsessive compulsive disorders | 1.35[1.14, 1.62] (1.46[1.14, 1.87]) | 0.003 (0.011) |
| Number of comorbidities | 0.77[0.68, 0.86] (0.76[0.65, 0.90]) | <0.001 (0.004) |
| Only anxiety disorder v.s Depressive and anxiety disorder | 0.74[0.59, 0.93] | 0.038 |
| Only bipolar disorder v.s No psychotic or bipolar disorder | 0.76[0.61, 0.94] | 0.042 |
| Number of depressive disorder episodes | 0.94[0.93, 0.95] (0.95[0.93, 0.96]) | <0.001 (<0.001) |
| Total duration on antidepressants | 1.01[1.00, 1.02] (1.01[1.00, 1.02]) | 0.002 (0.024) |
| Mean number of side effects | 0.93[0.92, 0.94] (0.93[0.91, 0.95]) | <0.001 (<0.001) |
| Side effect severity rating | 1.29[1.25, 1.32] (1.30[1.25, 1.36]) | <0.001 (<0.001) |
| Treatment discontinuation | 0.53[0.50, 0.57] (0.50[0.46, 0.55]) | <0.001 (<0.001) |


#### Table 2c: Variables significantly associated with number of best aspects
N = 7842
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.29[1.17, 1.43] | <0.001 |
| BMI (kg/m^2) | 1.01[1.00, 1.01] | 0.003 |
| Unable to work because of sickness or disability v.s In paid employment or self-employed | 0.57[0.50, 0.64] (0.50[0.42, 0.59]) | <0.001 (<0.001) |
| Unemployed v.s. In paid employment or self-employed | 0.67[0.54, 0.84] (0.68[0.51, 0.92]) | 0.001 (0.046) |
| In relationship v.s Not in relationship | 1.27[1.15, 1.40] (1.32[1.15, 1.52]) | <0.001 (<0.001) |
| Married v.s Not in relationship | 1.46[1.33, 1.61] (1.62[1.41, 1.87]) | <0.001 (<0.001) |
| Personality disorders | 0.76[0.63, 0.93] | 0.024 |
| Number of comorbidities | 0.86[0.77, 0.97] | 0.047 |
| Only anxiety disorder v.s Depressive and anxiety disorder | 0.51[0.41, 0.64] (0.52[0.38, 0.70]) | <0.001 (<0.001) |
| Only depressive disorder v.s Depressive and anxiety disorder | 0.69[0.58, 0.81] (0.64[0.51, 0.81]) | <0.001 (<0.001) |
| Number of depressive disorder episodes | 0.96[0.95, 0.97] (0.96[0.94, 0.97]) | <0.001 (<0.001) |
| Total duration on antidepressants | 1.02[1.01, 1.03] (1.02[1.01, 1.03]) | <0.001 (<0.001) |
| Respiratory & atopic conditions | 1.05[1.01, 1.08] | 0.024 |
| Side effect severity rating | 1.22[1.19, 1.26] (1.26[1.21, 1.31]) | <0.001 (<0.001) |
| Treatment discontinuation | 0.69[0.65, 0.74] (0.64[0.58, 0.70]) | <0.001 (<0.001) |
| Retired v.s In paid employment | (0.71[0.55, 0.91]) | (0.029) |


#### Table 2d: Variables significantly associated with occurrence of remission
N = 17457   [ADJUSTED COLUMN OMITTED]
| Parameter | OR [95% CI] | p (adjusted) |
|:--|:--|:--|
| Cumulative Medication Count | 0.91[0.89, 0.93] | <0.001 |
| Unable to work because of sickness or disability v.s In paid employment or self-employed | 0.59[0.51, 0.69] | <0.001 |
| Married v.s Not in relationship | 1.20[1.06, 1.36] | 0.017 |
| Personality disorders | 0.65[0.51, 0.82] | 0.001 |
| Only bipolar disorder v.s No psychotic or bipolar disorder | 0.62[0.48, 0.81] | 0.001 |
| Number of depressive disorder episodes | 0.95[0.94, 0.96] | <0.001 |
| Number of Side Effects | 1.03[1.02, 1.04] | <0.001 |
| Side effect severity rating | 1.16[1.12, 1.20] | <0.001 |
| Stopped due to Side Effects | 0.27[0.25, 0.30] | <0.001 |


#### Table 2e: Variables significantly associated with first improvement duration
N = 6556
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Average starting age/10 | 1.15[1.11, 1.20] | <0.001 |
| Number of relatives with psychiatric disorders | 0.97[0.96, 0.98] | <0.001 |
| Pack years of cigarettes smoked | 0.99[0.99, 1.00] (0.99[0.98, 0.99]) | 0.018 (<0.001) |
| Doing unpaid or voluntary work v.s. In paid employment or self-employed | 0.53[0.37, 0.76] (0.48[0.28, 0.82]) | 0.002 (0.028) |
| Full or part-time student v.s In paid employment or self-employed | 0.68[0.60, 0.78] | <0.001 |
| Retired v.s In paid employment | 1.43[1.18, 1.72] (1.52[1.11, 2.08]) | <0.001 (0.037) |
| Unable to work because of sickness or disability v.s In paid employment or self-employed | 0.47[0.41, 0.54] (0.51[0.43, 0.62]) | <0.001 (<0.001) |
| Unemployed v.s. In paid employment or self-employed | 0.67[0.52, 0.86] (0.63[0.44, 0.90]) | 0.008 (0.041) |
| Married v.s Not in relationship | 1.54[1.38, 1.71] (1.40[1.19, 1.65]) | <0.001 (<0.001) |
| Personality disorders | 0.59[0.48, 0.73] | <0.001 |
| Number of comorbidities | 0.84[0.74, 0.96] (0.79[0.66, 0.94]) | 0.036 (0.038) |
| Number of depressive disorder episodes | 0.93[0.92, 0.95] (0.94[0.92, 0.96]) | <0.001 (<0.001) |
| Total duration on antidepressants | 0.97[0.97, 0.98] (0.98[0.97, 0.98]) | <0.001 (<0.001) |
| Mean number of side effects | 0.96[0.94, 0.97] (0.95[0.94, 0.97]) | <0.001 (<0.001) |
| Side effect severity rating | 1.09[1.05, 1.12] (1.08[1.03, 1.13]) | <0.001 (0.007) |
| Treatment discontinuation | 0.60[0.56, 0.65] (0.61[0.55, 0.68]) | <0.001 (<0.001) |
