# Results tables — current numbers

Generated 2026-08-06 by `src/04.manuscript_tables.r` from the
cached model objects, under the **combination** decision: longitudinal (GLMM)
estimates for the four outcomes the GLMM covers, per-participant (legacy)
estimates for the four it does not.

Adjusted estimates appear in parentheses where the adjusted model is also
significant. Rows shown only in parentheses are significant after adjustment
but not before. The longitudinal models are adjusted for sex, starting age,
cumulative medication count and PHQ-9; the per-participant models for sex,
average starting age and number of relatives with a psychiatric disorder.


#### Table 1a: Variables significantly associated with the number of side effects
N = 25,453 medication observations from 8,011 participants. Models vary between 14,964 and 25,453 observations because each is fitted on complete cases.
| Parameter | RR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.20[1.12, 1.29] | <0.001 |
| Start Age | 1.00[1.00, 1.00] | 0.010 |
| Number of relatives with psychiatric disorders | 1.02[1.02, 1.03] (1.02[1.01, 1.02]) | <0.001 (<0.001) |
| BMI (kg/m^2) | 1.00[0.99, 1.00] (0.99[0.99, 1.00]) | 0.439 (<0.001) |
| Alcohol use disorder | 1.01[1.00, 1.01] (1.01[1.00, 1.01]) | 0.008 (0.056) |
| Pack years of cigarettes smoked | 0.99[0.99, 1.00] (0.99[0.99, 0.99]) | <0.001 (<0.001) |
| Full or part-time student vs. In paid employment or self-employed | 1.29[1.18, 1.41] (1.22[1.12, 1.34]) | <0.001 (<0.001) |
| Retired vs. In paid employment | 0.54[0.48, 0.61] (0.56[0.49, 0.63]) | <0.001 (<0.001) |
| Unable to work because of sickness or disability vs. In paid employment or self-employed | 1.20[1.10, 1.30] (0.98[0.90, 1.07]) | <0.001 (1.000) |
| In relationship vs. Not in relationship | 1.21[1.13, 1.30] (1.24[1.15, 1.32]) | <0.001 (<0.001) |
| Married vs. Not in relationship | 0.88[0.82, 0.94] (0.94[0.88, 1.01]) | 0.001 (0.389) |
| Obsessive compulsive disorders | 1.15[1.06, 1.25] (1.10[1.01, 1.19]) | 0.004 (0.111) |
| Only anxiety disorder vs. Depressive and anxiety disorder | 0.75[0.65, 0.86] (0.86[0.75, 0.99]) | <0.001 (0.126) |
| Only depressive disorder vs. Depressive and anxiety disorder | 0.68[0.63, 0.74] (0.73[0.67, 0.79]) | <0.001 (<0.001) |
| Only bipolar disorder vs. No psychotic or bipolar disorder | 1.16[1.03, 1.30] (1.16[1.04, 1.30]) | 0.049 (0.032) |
| Number of comorbidities | 1.18[1.15, 1.21] (1.12[1.09, 1.15]) | <0.001 (<0.001) |
| Number of depressive disorder episodes | 1.03[1.02, 1.04] (1.01[1.00, 1.02]) | <0.001 (0.115) |
| Cardiometabolic & endocrine Disorders | 0.87[0.83, 0.90] (0.85[0.81, 0.88]) | <0.001 (<0.001) |
| Autoimmune & inflammatory Diseases | 1.13[1.07, 1.18] (1.10[1.05, 1.15]) | <0.001 (<0.001) |
| Respiratory & atopic conditions | 1.07[1.04, 1.09] (1.05[1.02, 1.07]) | <0.001 (0.001) |
| Oncological disorders | 0.70[0.56, 0.88] (0.66[0.53, 0.82]) | 0.009 (<0.001) |
| Musculoskeletal & pain disorders | 0.93[0.88, 1.00] (0.90[0.85, 0.96]) | 0.138 (0.006) |

#### Table 1b: Variables significantly associated with side effect severity rating
N = 7,803 participants. Models vary between 3,857 and 7,803 participants because each is fitted on complete cases.
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.21[1.10, 1.34] | <0.001 |
| Average starting age/10 | 0.91[0.87, 0.94] | <0.001 |
| Pack years of cigarettes smoked | 0.99[0.99, 1.00] (1.00[0.99, 1.00]) | 0.010 (1.000) |
| Total duration on antidepressants | 1.02[1.01, 1.02] (1.02[1.01, 1.03]) | <0.001 (<0.001) |
| Cardiometabolic & endocrine Disorders | 0.91[0.87, 0.96] (0.96[0.88, 1.04]) | 0.004 (1.000) |

#### Table 1c: Variables significantly associated with stopping a medication because of side effects
N = 20,260 medication observations from 7,685 participants. Models vary between 12,228 and 20,260 observations because each is fitted on complete cases. Participant random-intercept ICC 0.37.
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.26[1.11, 1.42] | <0.001 |
| Start Age | 0.98[0.98, 0.98] | <0.001 |
| Number of relatives with psychiatric disorders | 1.04[1.03, 1.05] (1.03[1.02, 1.04]) | <0.001 (<0.001) |
| Retired vs. In paid employment | 0.69[0.56, 0.84] (1.07[0.86, 1.33]) | <0.001 (1.000) |
| Unable to work because of sickness or disability vs. In paid employment or self-employed | 1.64[1.44, 1.88] (1.29[1.12, 1.48]) | <0.001 (0.002) |
| Married vs. Not in relationship | 0.84[0.74, 0.94] (1.07[0.95, 1.21]) | 0.011 (1.000) |
| Obsessive compulsive disorders | 1.25[1.09, 1.43] (1.15[1.00, 1.32]) | 0.005 (0.190) |
| Personality disorders | 1.48[1.27, 1.72] (1.23[1.05, 1.44]) | <0.001 (0.037) |
| Only anxiety disorder vs. Depressive and anxiety disorder | 0.61[0.47, 0.79] (0.71[0.55, 0.93]) | <0.001 (0.055) |
| Only depressive disorder vs. Depressive and anxiety disorder | 0.54[0.46, 0.62] (0.59[0.51, 0.68]) | <0.001 (<0.001) |
| Only bipolar disorder vs. No psychotic or bipolar disorder | 1.78[1.49, 2.14] (1.75[1.46, 2.11]) | <0.001 (<0.001) |
| Number of comorbidities | 1.39[1.33, 1.44] (1.27[1.22, 1.33]) | <0.001 (<0.001) |
| Number of depressive disorder episodes | 1.09[1.07, 1.10] (1.07[1.05, 1.08]) | <0.001 (<0.001) |
| Cardiometabolic & endocrine Disorders | 0.92[0.86, 0.98] (0.99[0.93, 1.06]) | 0.035 (1.000) |
| Neurological conditions | 1.31[1.19, 1.45] (1.27[1.15, 1.40]) | <0.001 (<0.001) |
| Autoimmune & inflammatory Diseases | 1.16[1.07, 1.25] (1.11[1.03, 1.20]) | 0.001 (0.039) |
| Respiratory & atopic conditions | 1.14[1.10, 1.19] (1.13[1.08, 1.17]) | <0.001 (<0.001) |
| Oncological disorders | 0.60[0.41, 0.87] (0.76[0.52, 1.12]) | 0.032 (0.650) |
| Musculoskeletal & pain disorders | 1.06[0.95, 1.17] (1.19[1.07, 1.32]) | 1.000 (0.005) |

#### Table 2a: Variables significantly associated with effectiveness
N = 22,999 medication observations from 7,706 participants. Models vary between 13,653 and 22,999 observations because each is fitted on complete cases. Participant random-intercept ICC 0.11.
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.15[1.07, 1.24] | 0.001 |
| Start Age | 1.02[1.02, 1.02] | <0.001 |
| BMI (kg/m^2) | 1.00[1.00, 1.01] (1.01[1.00, 1.01]) | 0.168 (0.015) |
| Pack years of cigarettes smoked | 1.00[0.99, 1.00] (0.99[0.99, 1.00]) | 0.032 (<0.001) |
| Doing unpaid or voluntary work vs. In paid employment or self-employed | 0.64[0.51, 0.80] (0.65[0.52, 0.82]) | <0.001 (<0.001) |
| Full or part-time student vs. In paid employment or self-employed | 0.80[0.72, 0.88] (1.08[0.97, 1.19]) | <0.001 (0.635) |
| Retired vs. In paid employment | 1.13[1.01, 1.28] (0.75[0.66, 0.85]) | 0.159 (<0.001) |
| Unable to work because of sickness or disability vs. In paid employment or self-employed | 0.56[0.51, 0.61] (0.72[0.66, 0.79]) | <0.001 (<0.001) |
| Unemployed vs. In paid employment or self-employed | 0.69[0.59, 0.82] (0.85[0.72, 1.01]) | <0.001 (0.269) |
| In relationship vs. Not in relationship | 1.13[1.05, 1.22] (1.13[1.05, 1.21]) | 0.005 (0.007) |
| Married vs. Not in relationship | 1.40[1.31, 1.51] (1.08[1.00, 1.16]) | <0.001 (0.165) |
| Eating disorders | 0.86[0.79, 0.94] (0.93[0.85, 1.02]) | 0.005 (0.422) |
| ADHD | 0.70[0.58, 0.84] (0.82[0.68, 0.99]) | <0.001 (0.158) |
| Obsessive compulsive disorders | 0.88[0.81, 0.96] (0.94[0.86, 1.03]) | 0.010 (0.650) |
| Personality disorders | 0.63[0.58, 0.70] (0.77[0.70, 0.84]) | <0.001 (<0.001) |
| Autism spectrum disorders | 0.69[0.59, 0.80] (0.90[0.77, 1.05]) | <0.001 (0.757) |
| Only anxiety disorder vs. Depressive and anxiety disorder | 1.62[1.37, 1.92] (1.34[1.13, 1.60]) | <0.001 (0.003) |
| Only depressive disorder vs. Depressive and anxiety disorder | 1.24[1.14, 1.35] (1.12[1.02, 1.22]) | <0.001 (0.047) |
| Only bipolar disorder vs. No psychotic or bipolar disorder | 0.55[0.49, 0.62] (0.56[0.50, 0.63]) | <0.001 (<0.001) |
| Psychotic and bipolar disorder vs. No psychotic or bipolar disorder | 0.69[0.55, 0.86] (0.69[0.55, 0.86]) | 0.003 (0.004) |
| Number of comorbidities | 0.78[0.76, 0.80] (0.86[0.84, 0.88]) | <0.001 (<0.001) |
| Number of depressive disorder episodes | 0.93[0.92, 0.94] (0.95[0.95, 0.96]) | <0.001 (<0.001) |
| Neurological conditions | 0.92[0.87, 0.98] (0.93[0.88, 0.99]) | 0.028 (0.066) |
| Respiratory & atopic conditions | 0.96[0.94, 0.99] (0.97[0.95, 0.99]) | 0.007 (0.068) |
| Musculoskeletal & pain disorders | 0.97[0.91, 1.04] (0.88[0.83, 0.94]) | 1.000 (<0.001) |
| Number of Side Effects | 0.96[0.96, 0.97] (0.98[0.97, 0.99]) | <0.001 (<0.001) |
| Side effect severity rating | 1.08[1.05, 1.10] (1.07[1.05, 1.10]) | <0.001 (<0.001) |
| Stopped due to Side Effects | 0.08[0.07, 0.09] (0.08[0.08, 0.09]) | <0.001 (<0.001) |

#### Table 2b: Variables significantly associated with benefit rating
N = 7,741 participants. Models vary between 3,820 and 7,741 participants because each is fitted on complete cases.
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.56[1.41, 1.73] | <0.001 |
| Average starting age/10 | 1.09[1.05, 1.13] | <0.001 |
| BMI (kg/m^2) | 1.02[1.01, 1.02] (1.02[1.01, 1.03]) | <0.001 (<0.001) |
| Alcohol use disorder | 0.99[0.98, 1.00] (0.99[0.98, 1.00]) | 0.005 (1.000) |
| Doing unpaid or voluntary work vs. In paid employment or self-employed | 0.55[0.40, 0.76] (0.38[0.24, 0.60]) | 0.001 (<0.001) |
| Full or part-time student vs. In paid employment or self-employed | 0.69[0.61, 0.78] (0.74[0.61, 0.89]) | <0.001 (0.008) |
| Unable to work because of sickness or disability vs. In paid employment or self-employed | 0.54[0.48, 0.61] (0.52[0.44, 0.62]) | <0.001 (<0.001) |
| Unemployed vs. In paid employment or self-employed | 0.52[0.42, 0.65] (0.50[0.37, 0.68]) | <0.001 (<0.001) |
| In relationship vs. Not in relationship | 1.16[1.05, 1.28] (1.17[1.01, 1.35]) | 0.018 (0.142) |
| Married vs. Not in relationship | 1.54[1.39, 1.70] (1.54[1.33, 1.78]) | <0.001 (<0.001) |
| Obsessive compulsive disorders | 1.35[1.14, 1.62] (1.46[1.14, 1.87]) | 0.003 (0.011) |
| Number of comorbidities | 0.77[0.68, 0.86] (0.76[0.65, 0.90]) | <0.001 (0.004) |
| Only anxiety disorder vs. Depressive and anxiety disorder | 0.74[0.59, 0.93] (0.71[0.51, 0.98]) | 0.038 (0.154) |
| Only bipolar disorder vs. No psychotic or bipolar disorder | 0.76[0.61, 0.94] (0.73[0.54, 0.97]) | 0.042 (0.125) |
| Number of depressive disorder episodes | 0.94[0.93, 0.95] (0.95[0.93, 0.96]) | <0.001 (<0.001) |
| Total duration on antidepressants | 1.01[1.00, 1.02] (1.01[1.00, 1.02]) | 0.002 (0.024) |
| Mean number of side effects | 0.93[0.92, 0.94] (0.93[0.91, 0.95]) | <0.001 (<0.001) |
| Side effect severity rating | 1.29[1.25, 1.32] (1.30[1.25, 1.36]) | <0.001 (<0.001) |
| Treatment discontinuation | 0.53[0.50, 0.57] (0.50[0.46, 0.55]) | <0.001 (<0.001) |

#### Table 2c: Variables significantly associated with the number of best aspects
N = 7,842 participants. Models vary between 3,868 and 7,842 participants because each is fitted on complete cases.
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Sex (female) | 1.29[1.17, 1.43] | <0.001 |
| BMI (kg/m^2) | 1.01[1.00, 1.01] (1.01[1.00, 1.02]) | 0.003 (0.110) |
| Retired vs. In paid employment | 0.92[0.79, 1.08] (0.71[0.55, 0.91]) | 1.000 (0.029) |
| Unable to work because of sickness or disability vs. In paid employment or self-employed | 0.57[0.50, 0.64] (0.50[0.42, 0.59]) | <0.001 (<0.001) |
| Unemployed vs. In paid employment or self-employed | 0.67[0.54, 0.84] (0.68[0.51, 0.92]) | 0.001 (0.046) |
| In relationship vs. Not in relationship | 1.27[1.15, 1.40] (1.32[1.15, 1.52]) | <0.001 (<0.001) |
| Married vs. Not in relationship | 1.46[1.33, 1.61] (1.62[1.41, 1.87]) | <0.001 (<0.001) |
| Personality disorders | 0.76[0.63, 0.93] (0.71[0.55, 0.93]) | 0.024 (0.053) |
| Number of comorbidities | 0.86[0.77, 0.97] (0.84[0.72, 0.99]) | 0.047 (0.143) |
| Only anxiety disorder vs. Depressive and anxiety disorder | 0.51[0.41, 0.64] (0.52[0.38, 0.70]) | <0.001 (<0.001) |
| Only depressive disorder vs. Depressive and anxiety disorder | 0.69[0.58, 0.81] (0.64[0.51, 0.81]) | <0.001 (<0.001) |
| Number of depressive disorder episodes | 0.96[0.95, 0.97] (0.96[0.94, 0.97]) | <0.001 (<0.001) |
| Total duration on antidepressants | 1.02[1.01, 1.03] (1.02[1.01, 1.03]) | <0.001 (<0.001) |
| Respiratory & atopic conditions | 1.05[1.01, 1.08] (1.02[0.98, 1.07]) | 0.024 (1.000) |
| Side effect severity rating | 1.22[1.19, 1.26] (1.26[1.21, 1.31]) | <0.001 (<0.001) |
| Treatment discontinuation | 0.69[0.65, 0.74] (0.64[0.58, 0.70]) | <0.001 (<0.001) |

#### Table 2d: Variables significantly associated with occurrence of remission
N = 17,457 medication observations from 7,402 participants. Models vary between 10,365 and 17,457 observations because each is fitted on complete cases. Participant random-intercept ICC 0.43.
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Cumulative Medication Count | 0.91[0.89, 0.93] | <0.001 |
| Unable to work because of sickness or disability vs. In paid employment or self-employed | 0.59[0.51, 0.69] (0.73[0.63, 0.85]) | <0.001 (<0.001) |
| Married vs. Not in relationship | 1.20[1.06, 1.36] (1.08[0.95, 1.22]) | 0.017 (1.000) |
| Personality disorders | 0.54[0.46, 0.64] (0.62[0.52, 0.74]) | <0.001 (<0.001) |
| Autism spectrum disorders | 0.67[0.51, 0.87] (0.79[0.61, 1.04]) | 0.013 (0.380) |
| Only anxiety disorder vs. Depressive and anxiety disorder | 1.45[1.10, 1.90] (1.26[0.96, 1.66]) | 0.030 (0.394) |
| Only depressive disorder vs. Depressive and anxiety disorder | 1.31[1.13, 1.51] (1.24[1.07, 1.43]) | 0.001 (0.014) |
| Only bipolar disorder vs. No psychotic or bipolar disorder | 0.54[0.43, 0.66] (0.55[0.45, 0.68]) | <0.001 (<0.001) |
| Number of comorbidities | 0.76[0.73, 0.80] (0.81[0.77, 0.85]) | <0.001 (<0.001) |
| Number of depressive disorder episodes | 0.95[0.94, 0.96] (0.97[0.96, 0.98]) | <0.001 (<0.001) |
| Autoimmune & inflammatory Diseases | 1.10[1.01, 1.20] (1.13[1.04, 1.23]) | 0.148 (0.015) |
| Number of Side Effects | 1.03[1.02, 1.04] (1.04[1.03, 1.05]) | <0.001 (<0.001) |
| Side effect severity rating | 1.16[1.12, 1.20] (1.17[1.13, 1.21]) | <0.001 (<0.001) |
| Stopped due to Side Effects | 0.27[0.25, 0.30] (0.29[0.26, 0.32]) | <0.001 (<0.001) |

#### Table 2e: Variables significantly associated with first improvement duration
N = 6,556 participants. Models vary between 3,216 and 6,556 participants because each is fitted on complete cases.
| Parameter | OR [95% CI] (adjusted) | p (adjusted) |
|:--|:--|:--|
| Average starting age/10 | 1.15[1.11, 1.20] | <0.001 |
| Number of relatives with psychiatric disorders | 0.97[0.96, 0.98] | <0.001 |
| Pack years of cigarettes smoked | 0.99[0.99, 1.00] (0.99[0.98, 0.99]) | 0.018 (<0.001) |
| Doing unpaid or voluntary work vs. In paid employment or self-employed | 0.53[0.37, 0.76] (0.48[0.28, 0.82]) | 0.002 (0.028) |
| Full or part-time student vs. In paid employment or self-employed | 0.68[0.60, 0.78] (0.81[0.66, 0.99]) | <0.001 (0.170) |
| Retired vs. In paid employment | 1.43[1.18, 1.72] (1.52[1.11, 2.08]) | <0.001 (0.037) |
| Unable to work because of sickness or disability vs. In paid employment or self-employed | 0.47[0.41, 0.54] (0.51[0.43, 0.62]) | <0.001 (<0.001) |
| Unemployed vs. In paid employment or self-employed | 0.67[0.52, 0.86] (0.63[0.44, 0.90]) | 0.008 (0.041) |
| Married vs. Not in relationship | 1.54[1.38, 1.71] (1.40[1.19, 1.65]) | <0.001 (<0.001) |
| Personality disorders | 0.59[0.48, 0.73] (0.72[0.53, 0.96]) | <0.001 (0.104) |
| Number of comorbidities | 0.84[0.74, 0.96] (0.79[0.66, 0.94]) | 0.036 (0.038) |
| Number of depressive disorder episodes | 0.93[0.92, 0.95] (0.94[0.92, 0.96]) | <0.001 (<0.001) |
| Total duration on antidepressants | 0.97[0.97, 0.98] (0.98[0.97, 0.98]) | <0.001 (<0.001) |
| Mean number of side effects | 0.96[0.94, 0.97] (0.95[0.94, 0.97]) | <0.001 (<0.001) |
| Side effect severity rating | 1.09[1.05, 1.12] (1.08[1.03, 1.13]) | <0.001 (0.007) |
| Treatment discontinuation | 0.60[0.56, 0.65] (0.61[0.55, 0.68]) | <0.001 (<0.001) |
