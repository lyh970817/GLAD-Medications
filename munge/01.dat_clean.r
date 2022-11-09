# Sex
sex_med <- sex_gender_sexuality_glad_clean %>%
  id_select(sex = dem.sex)

# Age
age_med <- age_glad_clean %>%
  mutate(age = dem.dob_age / 10) %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(age)

# BMI
# No startDate in the file?
signup_bmi_height_weight_med <- signup_bmi_height_weight_glad_clean %>%
  na_convert() %>%
  id_select(bmi = dem.bmi_signup)

# Education
years_of_education_med <- years_of_education_glad_clean %>%
  na_convert() %>%
  # Missing 'startDate' and 'endDate'
  # na_row_remove() %>%
  id_select(education_yrs = dem.years_schoolplease_include_preschool.txt)

# Employment
recode_empolyed <- c(
  "In paid employment or self-employed",
  "Retired",
  "Looking after home and/or family",
  "Doing unpaid or voluntary work",
  "Full or part-time student",
  "None of the above"
) %>%
  setNames(rep("Employed", length(.)))

recode_unempolyed <-
  c(
    "Unemployed",
    "Unable to work because of sickness or disability"
  ) %>%
  setNames(rep("Unemployed", length(.)))

employment_glad_clean$dem.what_is_your_current_employment_status <-
  employment_glad_clean$dem.what_is_your_current_employment_status %>%
  fct_recode(!!!recode_empolyed) %>%
  fct_recode(!!!recode_unempolyed) %>%
  factor(levels = c("Unemployed", "Employed"))

employment_med <- employment_glad_clean %>%
  na_convert() %>%
  # Missing 'startDate' and 'endDate'
  # na_row_remove() %>%
  id_select(employment = dem.what_is_your_current_employment_status)

# Marital status
recode_unmarried <-
  c(
    "Single",
    "Divorced",
    "Widowed",
    "Separated"
  ) %>%
  setNames(rep("Unmaried", length(.)))


recode_married <-
  c(
    # Separated into three levels
    "Married",
    "Married/civil partnership",

    # Qualtrics error
    "Relationship (living together)",
    "Relationship (not living together)",
    # Combine with the above
    "Steady living together",
    "Steady not living together"
  ) %>%
  setNames(rep("Married", length(.)))


marital_status_glad_clean$dem.what_is_your_current_maritalrelationship_status <-
  marital_status_glad_clean$dem.what_is_your_current_maritalrelationship_status %>%
  fct_recode(!!!recode_married) %>%
  fct_recode(!!!recode_unmarried) %>%
  fct_recode(NULL = "Other")

marital_status_med <- marital_status_glad_clean %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(marital = dem.what_is_your_current_maritalrelationship_status)


# MHD
mhd_med <- mhd_glad_clean_tmp %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(
    depression_and_anxiety,
    bipolar_and_schizophrenia,
    eating_disorders_numeric,
    # Suggest a change in uniform names (dot/slash) in ilovedata?
    mhd_addadhd_numeric = mhd.addadhd_numeric,
    obsessive_compulsive_disorders_numeric,
    mhd_personality_disorder_numeric = mhd.personality_disorder_numeric,
    autism_spectrum_disorder_numeric,
    comorbidity_total_count_numeric
  )

# AUDIT
audit_med <- audit_glad_clean %>%
  id_select(audit = audit.sum_score)

# CIDID recurrence
cidid_recurrence_med <- cidid_recurrence_glad_clean %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(cidid_recurrence = cidid.number_of_episodes_numeric)

# CIDIA recurrence
# Wrong!!! Too many NAs
# sum(is.na(cidia_recurrence_med$cidia_recurrence))
cidia_recurrence_med <- cidia_cleaning_algorithm_glad_clean %>%
  id_select(cidia_recurrence = cidia.number_of_episodes)

# GAD7
gad7_med <- gad7_glad_clean %>%
  id_select(gad7 = gad7.sum_score)

# PHQ9
phq9_med <- phq9_glad_clean %>%
  id_select(phq9 = phq9.sum_score, phq9_bin = phq9.binary_depression_numeric)

# WSAS
wsas_med <- wsas_glad_clean %>%
  id_select(wsas = wsas.sum_score)

# Number of relatives wtih psychiatric disorder
# Needs updating ilovedata script

n_relatives <- fh_mhd_f_glad_dat %>%
  full_join(fh_mhd2_f_glad_dat) %>%
  select(ID = externalDataReference, everything()) %>%
  mutate(sample = "glad") %>%
  mutate(across(starts_with("fh_"), str_extract, "\\d*")) %>%
  mutate(across(starts_with("fh_"), as.numeric)) %>%
  # All to positive
  mutate(across(starts_with("fh_"), abs)) %>%
  na_convert() %>%
  na_row_remove() %>%
  rowwise() %>%
  mutate(n_relatives = sum(c_across(starts_with("fh_")), na.rm = T)) %>%
  id_select(n_relatives)

# Smoking
smoking_pack_year <- smoking_pack_year_glad %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(pack_year = dem.pack_year)

# Number of benefits
n_best <-
  antidepressants_ben_glad_med_id %>%
  na_convert() %>%
  na_row_remove() %>%
  rowwise() %>%
  mutate(
    n_best =
      sum(c_across(
        antidepressants_ben.relief_of_depressive_symptoms:
        antidepressants_ben.other
      ), na.rm = T)
  ) %>%
  ungroup() %>%
  id_select(n_best) %>%
  mutate(n_best = factor(n_best, ordered = TRUE))


# Overall benefit rating
ben_rating <- antidepressants_ben_glad_med_id %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(ben_rating = antidepressants_ben.benefits_rate_taking_antidepressantss) %>%
  mutate(ben_rating = factor(ben_rating, ordered = TRUE))

# Overall side effect rating
se_rating <- sideeffect_rating %>%
  na_convert() %>%
  id_select(se_rating = sideeffects.rate_sideeffects_taking_antidepressants) %>%
  mutate(se_rating = factor(se_rating, ordered = TRUE))

# Number of medications
prescription_antidepressants_id$prescription.a_different_antidepressants <- NULL

n_meds <- prescription_antidepressants_id %>%
  na_convert() %>%
  na_row_remove() %>%
  rowwise() %>%
  mutate(n_meds = sum(c_across(prescription.citalopram:prescription.vortioxetine))) %>%
  id_select(n_meds) %>%
  ungroup()

# Mean number of side effects
sideeffects_antidepressants_id <- sideeffects_antidepressants_id %>%
  select(-contains("a_different_antidepressants"))

mean_n_se <- sideeffects_antidepressants_id %>%
  select(-contains("side_effects_stop_taking")) %>%
  na_convert() %>%
  na_row_remove() %>%
  rowwise() %>%
  mutate(
    n_se =
      sum(c_across(
        sideeffects.dry_mouth.citalopram:sideeffects.other.vortioxetine
      ), na.rm = T)
  ) %>%
  id_select(n_se) %>%
  left_join(n_meds) %>%
  mutate(mean_n_se = n_se / n_meds) %>%
  id_select(mean_n_se) %>%
  ungroup()

# Likelihood of intolerance
intolerance <- sideeffects_antidepressants_id %>%
  id_select(sample, startDate, endDate, contains("side_effects_stop_taking")) %>%
  na_convert() %>%
  na_row_remove() %>%
  rowwise() %>%
  mutate(
    intolerance_count =
      sum(c_across(
        sideeffects.side_effects_stop_taking.citalopram:
        sideeffects.side_effects_stop_taking.vortioxetine
      ), na.rm = T)
  ) %>%
  ungroup() %>%
  left_join(n_meds, by = "ID") %>%
  mutate(intolerance = intolerance_count / n_meds) %>%
  id_select(intolerance)

# Mean efficacy

mean_eff <- antidepressants_eff_glad_med_id %>%
  select(-contains("a_different_antidepressants")) %>%
  na_convert() %>%
  na_row_remove() %>%
  rowwise() %>%
  mutate(
    total_eff =
      sum(c_across(antidepressants_eff.antidepressants_work_doesdid.citalopram:
      antidepressants_eff.antidepressants_work_doesdid.vortioxetine), na.rm = T)
  ) %>%
  ungroup() %>%
  left_join(n_meds) %>%
  mutate(mean_eff = total_eff / n_meds) %>%
  id_select(mean_eff)

antidepressants_why_glad_med_id <- antidepressants_why_glad_med_id %>%
  select(-contains("a_different_antidepressants")) %>%
  # Assign to Molly - none shouldn't be here
  select(-contains("none")) %>%
  na_convert() %>%
  na_row_remove()

# Age when starting to take antidepressants
started_age <- antidepressants_why_glad_med_id %>%
  id_select(contains("started_taking")) %>%
  mutate_at(vars(contains("started_taking")), function(x) {
    x[x < 5 | x > 100] <- NA
    x
  })

# First improvement duration and likelihood of remission
antidepressants_imprv_glad_med_id <- antidepressants_imprv_glad_med_id %>%
  select(-contains("a_different_antidepressants")) %>%
  na_convert() %>%
  na_row_remove()


# First improvement duration
started_age_t <- started_age %>%
  left_join(antidepressants_imprv_glad_med_id["ID"], .) %>%
  select(-ID) %>%
  transpose()

imprv_t <- antidepressants_imprv_glad_med_id %>%
  select(contains("experienced_improvement_symptoms_long")) %>%
  transpose()

first_imprv <- map2_dbl(
  started_age_t, imprv_t,
  function(age, imprv) {
    imprv <- unlist(imprv)
    age <- unlist(age)
    imprv_by_age <- imprv[order(age)]

    # First non-NA value
    imprv_by_age[!is.na(imprv_by_age)][1]
  }
) %>%
  tibble(antidepressants_imprv_glad_med_id["ID"], first_imprv = .) %>%
  mutate(first_imprv = factor(first_imprv, ordered = TRUE))

# browser()
# remission[remission$remission > 1,"n_meds"]
# remission[which(remission$remission > 1)[1],] %>%
#     unlist()
# remission[remission$remission > 1,"remission_count"]
# prescription_antidepressants_id[prescription_antidepressants_id$ID == "8f552fcb8136789a50be71328410",] %>%
#     unlist()
# No St john's wort?

# Likelihood of remission
remission <- antidepressants_imprv_glad_med_id %>%
  id_select(contains("condition_period_time_experience")) %>%
  rowwise() %>%
  mutate(
    remission_count =
      sum(c_across(
        antidepressants_imprv.condition_period_time_experience.citalopram:
        antidepressants_imprv.condition_period_time_experience.vortioxetine
      ),
      na.rm = T
      )
  ) %>%
  ungroup() %>%
  left_join(n_meds, by = "ID") %>%
  mutate(remission = remission_count / n_meds) %>%
  id_select(remission)


# Time on antidepressants

# No question in str here???
# str(antidepressants_why_glad_med_id[648])

time <- antidepressants_why_glad_med_id %>%
  # no space in easy name
  id_select(contains("long_taking_takehave")) %>%
  rowwise() %>%
  mutate(
    time =
      sum(c_across(
        antidepressants_why.long_taking_takehave.citalopram:
        antidepressants_why.long_taking_takehave.vortioxetine
      ), na.rm = T)
  ) %>%
  id_select(time)


dat_list <- list(
  n_meds,
  sex_med,
  age_med,
  years_of_education_med,
  employment_med,
  signup_bmi_height_weight_med,
  marital_status_med,
  mhd_med,
  audit_med,
  cidid_recurrence_med,
  # cidia_recurrence_med,
  gad7_med,
  phq9_med,
  wsas_med,
  n_relatives,
  smoking_pack_year,
  n_best,
  ben_rating,
  se_rating,
  mean_n_se,
  intolerance,
  mean_eff,
  first_imprv,
  remission,
  time
)

dat <- reduce(dat_list, left_join) %>%
  filter(depression_and_anxiety != "No depressive or anxiety disorder") %>%
  # Remove unused levels
  mutate_if(is.factor, droplevels)


labels <- c(
  "Number of antidepressantss",
  "Sex",
  "Age/10",
  "Years of education",
  "Employment",
  "BMI (kg/m^2)",
  "Marital status",
  "Depressive and anxiety disorders",
  "Psychotic and bipolar disorders",
  "Eating disorders",
  "ADHD",
  "Obsessive compulsive disorders",
  "Personality disorders",
  "Autism spectrum disorders",
  "Number of comorbidities",
  "### AUDIT score",
  "Number of depressive disorder episodes",
  "Current anxiety",
  "Current depression",
  "Current depression binary",
  "Work and social adjustment",
  "Number of relatives with psychiatric disorders",
  "Pack years of cigarettes smoked",
  "Number of best aspects",
  "Overall benefits rating",
  "Overall side effects rating",
  "Mean number of side effects",
  "Likelihood of intolerance",
  "Average self-report efficacy",
  "First improvement duration",
  "Likelihood of remission",
  "Time on antidepressantss"
) %>% setNames(colnames(dat)[-1])

cache("labels")

dat_uncut <- dat

cache("dat_uncut")

dat <- dat %>%
  mutate(
    mean_eff = cut(mean_eff, 6, labels = 1:6, ordered_result = T),
    remission = cut(remission, 3, labels = 1:3, ordered_result = T),
    intolerance = cut(intolerance, 3, labels = 1:3, ordered_result = T)
  )

cache("dat")
