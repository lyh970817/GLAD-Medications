require(fastDummies)

# Sex
sex_med <- sex_gender_sexuality_glad_clean %>%
  id_select(sex = dem.sex)

# Age
age_med <- age_glad_clean %>%
  dplyr::mutate(age = dem.dob_age / 10) %>%
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
  id_select(education_yrs = `dem.years_schoolplease_include_preschool.txt`)

# Employment
employment_glad_clean$dem.what_is_your_current_employment_status <-
  employment_glad_clean$dem.what_is_your_current_employment_status %>%
  factor()

employment_med <- employment_glad_clean %>%
  na_convert() %>%
  # Missing 'startDate' and 'endDate'
  id_select(
    "In_paid_employment_or_self-employed" =
      dem.what_is_your_current_employment_status
  ) %>%
  dummy_cols(
    select_columns = "In_paid_employment_or_self-employed",
    remove_most_frequent_dummy = TRUE
  ) %>%
  mutate_at(
    vars(contains("In_paid_employment_or_self-employed")),
    factor,
    levels = c(0, 1), labels = c("No", "Yes")
  ) %>%
  select(-"In_paid_employment_or_self-employed")

colnames(employment_med) <- gsub(" ", "_", colnames(employment_med))
# Hyphen will be confused with minus sign in R formulae
colnames(employment_med) <- gsub("-", "_", colnames(employment_med))
colnames(employment_med) <- gsub("/", "_", colnames(employment_med))

# Remove "None of the above" employment category from analysis
employment_med <- employment_med %>%
  select(-any_of("In_paid_employment_or_self_employed_None_of_the_above"))

# Marital status
recode_norelationship <-
  c(
    "Single",
    "Divorced",
    "Widowed",
    "Separated"
  ) %>%
  setNames(rep("Not in relationship", length(.)))


recode_married <-
  c(
    "Married",
    "Married/civil partnership"
  ) %>%
  setNames(rep("Married", length(.)))


recode_inrelationship <-
  c(
    # Qualtrics error
    "Relationship (living together)",
    "Relationship (not living together)"
    # Combine with the above
    # "Steady living together",
    # "Steady not living together"
  ) %>%
  setNames(rep("In relationship", length(.)))

marital_status_glad_clean$dem.what_is_your_current_maritalrelationship_status <-
  marital_status_glad_clean$dem.what_is_your_current_maritalrelationship_status %>%
  fct_recode(!!!recode_married) %>%
  fct_recode(!!!recode_norelationship) %>%
  fct_recode(!!!recode_inrelationship) %>%
  fct_recode(NULL = "Other") %>%
  factor()

marital_status_med <- marital_status_glad_clean %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(Not_in_relationship = dem.what_is_your_current_maritalrelationship_status) %>%
  dummy_cols(
    select_columns = "Not_in_relationship",
    remove_most_frequent_dummy = TRUE
  ) %>%
  select(-"Not_in_relationship")
colnames(marital_status_med) <- gsub(" ", "_", colnames(marital_status_med))

# MHD
mhd_med <- mhd_glad_clean_tmp %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(
    "Depressive_and_anxiety_disorder" = depression_and_anxiety,
    "No_psychotic_or_bipolar_disorder" = bipolar_and_schizophrenia,
    eating_disorders_numeric,
    # Suggest a change in uniform names (dot/slash) in ilovedata?
    mhd_addadhd_numeric = mhd.addadhd_numeric,
    obsessive_compulsive_disorders_numeric,
    mhd_personality_disorder_numeric = mhd.personality_disorder_numeric,
    autism_spectrum_disorder_numeric,
    comorbidity_total_count_numeric
  ) %>%
  dummy_cols(
    select_columns = "Depressive_and_anxiety_disorder",
    remove_most_frequent_dummy = TRUE
  ) %>%
  mutate_at(
    vars(contains("Depressive_and_anxiety_disorder")),
    factor,
    levels = c(0, 1), labels = c("No", "Yes")
  ) %>%
  select(-Depressive_and_anxiety_disorder) %>%
  dummy_cols(
    select_columns = "No_psychotic_or_bipolar_disorder",
    remove_most_frequent_dummy = TRUE
  ) %>%
  mutate_at(vars(contains("No_psychotic_or_bipolar_disorder")),
    factor,
    levels = c(0, 1), labels = c("No", "Yes")
  ) %>%
  select(-No_psychotic_or_bipolar_disorder)
colnames(mhd_med) <- gsub(" ", "_", colnames(mhd_med))

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
cidia_recurrence_med <- cidia_cleaning_algorithm_glad_clean %>%
  id_select(cidia_recurrence = cidia.number_of_episodes)

# GAD7
gad7_med <- gad7_glad_clean %>%
  id_select(gad7 = gad7.sum_score)

# PHQ9
phq9_med <- phq9_glad_clean %>%
  id_select(
    phq9 = phq9.sum_score
    # phq9_bin = phq9.binary_depression_numeric
  )

# WSAS
wsas_med <- wsas_glad_clean %>%
  id_select(wsas = wsas.sum_score)

# Number of relatives with psychiatric disorder
# Needs updating ilovedata script

n_relatives <- fh_mhd_f_glad_dat %>%
  full_join(fh_mhd2_f_glad_dat, by = c("externalDataReference", "startDate", "endDate")) %>%
  select(ID = externalDataReference, everything()) %>%
  dplyr::mutate(sample = "glad") %>%
  dplyr::mutate(across(starts_with("fh_"), str_extract, "\\d*")) %>%
  dplyr::mutate(across(starts_with("fh_"), as.numeric)) %>%
  # All to positive
  dplyr::mutate(across(starts_with("fh_"), abs)) %>%
  na_convert() %>%
  rowwise() %>%
  dplyr::mutate(n_relatives = sum(c_across(starts_with("fh_")), na.rm = T)) %>%
  ungroup() %>%
  id_select(n_relatives) %>%
  group_by(ID) %>%
  dplyr::summarise(n_relatives = max(n_relatives, na.rm = TRUE), .groups = "drop")

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
  dplyr::mutate(
    n_best =
      sum(c_across(
        antidepressants_ben.relief_of_depressive_symptoms:
        antidepressants_ben.other
      ), na.rm = T)
  ) %>%
  ungroup() %>%
  id_select(n_best) %>%
  dplyr::mutate(n_best = factor(n_best, ordered = TRUE))

# Overall benefit rating
ben_rating <- antidepressants_ben_glad_med_id %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(ben_rating = antidepressants_ben.benefits_rate_taking_antidepressantss) %>%
  dplyr::mutate(ben_rating = factor(ben_rating, ordered = TRUE))

# Overall side effect rating
se_rating <- sideeffect_rating %>%
  na_convert() %>%
  id_select(se_rating = sideeffects.rate_sideeffects_taking_antidepressants) %>%
  dplyr::mutate(se_rating = factor(se_rating, ordered = TRUE))

# Number of medications
prescription_antidepressants_id$prescription.a_different_antidepressants <- NULL

n_meds <- prescription_antidepressants_id %>%
  na_convert() %>%
  rowwise() %>%
  dplyr::mutate(n_meds = sum(c_across(prescription.citalopram:prescription.vortioxetine), na.rm = TRUE)) %>%
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
  dplyr::mutate(
    n_se =
      sum(c_across(
        sideeffects.dry_mouth.citalopram:sideeffects.other.vortioxetine
      ), na.rm = T)
  ) %>%
  id_select(n_se) %>%
  left_join(n_meds, by = "ID") %>%
  dplyr::mutate(mean_n_se = n_se / n_meds) %>%
  id_select(mean_n_se) %>%
  ungroup()

# Likelihood of intolerance
intolerance <- sideeffects_antidepressants_id %>%
  id_select(sample, startDate, endDate, contains("side_effects_stop_taking")) %>%
  na_convert() %>%
  na_row_remove() %>%
  rowwise() %>%
  dplyr::mutate(
    intolerance_count =
      sum(c_across(
        sideeffects.side_effects_stop_taking.citalopram:
        sideeffects.side_effects_stop_taking.vortioxetine
      ), na.rm = T)
  ) %>%
  ungroup() %>%
  left_join(n_meds, by = "ID") %>%
  dplyr::mutate(intolerance = intolerance_count / n_meds) %>%
  id_select(intolerance)

# Mean efficacy

mean_eff <- antidepressants_eff_glad_med_id %>%
  select(-contains("a_different_antidepressants")) %>%
  na_convert() %>%
  na_row_remove() %>%
  rowwise() %>%
  dplyr::mutate(
    total_eff =
      sum(c_across(antidepressants_eff.antidepressants_work_doesdid.citalopram:
      antidepressants_eff.antidepressants_work_doesdid.vortioxetine), na.rm = T)
  ) %>%
  ungroup() %>%
  left_join(n_meds, by = "ID") %>%
  dplyr::mutate(mean_eff = total_eff / n_meds) %>%
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

# Average starting age
avg_start_age <- started_age[-1] %>%
  rowMeans(na.rm = T) %>%
  bind_cols(started_age["ID"], avg_start_age = .) %>%
  dplyr::mutate(avg_start_age = avg_start_age / 10)

# First improvement duration and occurrence of remission
antidepressants_imprv_glad_med_id <- antidepressants_imprv_glad_med_id %>%
  na_convert() %>%
  na_row_remove()


# First improvement duration
started_age_t <- started_age %>%
  left_join(antidepressants_imprv_glad_med_id["ID"], ., by = "ID") %>%
  select(-ID) %>%
  purrr::transpose()

imprv_t <- antidepressants_imprv_glad_med_id %>%
  select(contains("experienced_improvement_symptoms_long")) %>%
  purrr::transpose()

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
  dplyr::mutate(first_imprv = factor(first_imprv, ordered = TRUE))

# Disabilities and illnesses
disability_illness_bin <- disability_illness_glad_clean[
  c("ID", "sample", "startDate", "endDate", "dem.disability_numeric")
] %>%
  na_convert() %>%
  na_row_remove()

illnesses <- c(
  # "dem.metal_implants_numeric",
  "dem.epilepsy_or_convulsions_numeric",
  "dem.migraines_numeric",
  "dem.multiple_sclerosis_numeric",
  # Rank deficient
  "dem.parkinsons_disease_numeric",
  "dem.severe_memory_loss_numeric",
  # "dem.neurological_none_numeric", "dem.neurological_dont_know_numeric",
  # "dem.neurological_prefer_not_to_answer_numeric",
  "dem.hay_fever_numeric",
  "dem.drug_allergy_numeric", "dem.food_allergy_numeric",
  "dem.other_allergy_numeric", "dem.osteoporosis_numeric",
  "dem.osteoarthritis_numeric", "dem.rheumatoid_arthritis_numeric",
  "dem.other_arthritis_numeric",
  # "dem.allergy_none_numeric",
  "dem.asthma_numeric", "dem.emphysema_or_chronic_bronchitis_numeric",
  "dem.heart_attack_or_angina_numeric", "dem.high_blood_cholesterol_numeric",
  "dem.high_blood_pressure_numeric", "dem.atrial_fibrillation_numeric",
  "dem.stroke_numeric",
  "dem.crohns_disease_numeric", "dem.ulcerative_colitis_numeric",
  "dem.coeliac_disease_numeric", "dem.diabetes_type_1_numeric",
  "dem.diabetes_type_2_numeric",
  # "dem.pain_due_to_diabetes_numeric",
  "dem.pain_due_to_virus_numeric",
  "dem.breast_cancer_numeric",
  # Rank deficient
  "dem.lung_cancer_numeric",
  "dem.stomach_cancer_numeric",
  "dem.colon_cancer_numeric",
  "dem.uterus_cancer_numeric",
  "dem.prostate_cancer_numeric",
  "dem.psoriasis_numeric",
  "dem.vitiligo_numeric", "dem.eczema_numeric",
  "dem.thyroid_disease_numeric"
  # "dem.listed_previously_told_illness_numeric"
  # All NA
  # "dem.brain_tumour_numeric",
  # "dem.ankylosing_spondylitis_numeric", "dem.hypermobility_numeric",
  # "dem.pots_numeric", "dem.diabetes_type_1_early_onset_numeric",
  # "dem.diabetes_type_1_late_onset_numeric",
  # "dem.diabetes_type_2_late_onset_numeric",
  # "dem.pcos_numeric"
)

cache("illnesses")

lab_illnesses <- c(
  # "Metal implants",
  "Epilepsy or convulsions",
  "Migraines", "Multiple sclerosis",
  # Rank deficient
  "Parkinsons disease",
  "Severe memory loss",
  # "Neurological none", "Neurological dont know",
  # "Neurological prefer not to answer",
  "Hay fever",
  "Drug allergy", "Food allergy",
  "Other allergy", "Osteoporosis",
  "Osteoarthritis", "Rheumatoid arthritis",
  "Other arthritis",
  # "Allergy none",
  "Asthma", "Emphysema or chronic bronchitis",
  "Heart attack or angina", "High blood cholesterol",
  "High blood pressure", "Atrial fibrillation",
  "Stroke",
  "Crohns disease", "Ulcerative colitis",
  "Coeliac disease", "Diabetes type 1",
  "Diabetes type 2",
  # "Pain due to diabetes",
  "Pain due to virus",
  "Breast cancer",
  # Rank deficient
  "Lung cancer",
  "Stomach cancer",
  "Colon cancer",
  "Uterus cancer", "Prostate cancer",
  "Psoriasis",
  "Vitiligo", "Eczema",
  "Thyroid disease"
  # "Listed previously told illness"
  # All NA
  # "Brain tumour",
  # "Ankylosing spondylitis", "Hypermobility",
  # "Pots", "Diabetes type 1 early onset",
  # "Diabetes type 1 late onset",
  # "Diabetes type 2 late onset",
  # "Pcos"
)

disability_illness_bin <- disability_illness_glad_clean[
  c("ID", "sample", "startDate", "endDate", "dem.disability_numeric")
] %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(disable_bin = dem.disability_numeric)

disabilities_illnesses <- disability_illness_glad_clean[
  c("ID", "sample", "startDate", "endDate", illnesses)
] %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(illnesses)


# 1. Define the Group Vectors
# (Based on the filtered list from the previous step)

group_cardiometabolic <- c(
  "dem.heart_attack_or_angina_numeric",
  "dem.high_blood_cholesterol_numeric",
  "dem.high_blood_pressure_numeric",
  "dem.atrial_fibrillation_numeric",
  "dem.stroke_numeric",
  "dem.diabetes_type_2_numeric",
  "dem.thyroid_disease_numeric"
)

group_neurological <- c(
  "dem.parkinsons_disease_numeric",
  "dem.epilepsy_or_convulsions_numeric",
  "dem.migraines_numeric",
  "dem.multiple_sclerosis_numeric",
  "dem.severe_memory_loss_numeric"
)

group_autoimmune <- c(
  "dem.rheumatoid_arthritis_numeric",
  "dem.crohns_disease_numeric",
  "dem.ulcerative_colitis_numeric",
  "dem.coeliac_disease_numeric",
  "dem.diabetes_type_1_numeric",
  "dem.psoriasis_numeric",
  "dem.vitiligo_numeric",
  "dem.eczema_numeric"
)

group_respiratory_atopic <- c(
  "dem.hay_fever_numeric",
  "dem.drug_allergy_numeric",
  "dem.food_allergy_numeric",
  "dem.other_allergy_numeric",
  "dem.asthma_numeric",
  "dem.emphysema_or_chronic_bronchitis_numeric" # Kept exact name from your list
)

group_oncology <- c(
  "dem.breast_cancer_numeric",
  "dem.colon_cancer_numeric",
  "dem.uterus_cancer_numeric",
  "dem.lung_cancer_numeric",
  "dem.stomach_cancer_numeric",
  "dem.prostate_cancer_numeric"
)

group_musculoskeletal_pain <- c(
  "dem.osteoporosis_numeric",
  "dem.osteoarthritis_numeric",
  "dem.other_arthritis_numeric",
  "dem.pain_due_to_virus_numeric"
)

# 2. Create the dataframe with Sum Scores

grouped_illnesses <- disability_illness_glad_clean[
  c("ID", "sample", "startDate", "endDate", illnesses)
] %>%
  na_convert() %>%
  na_row_remove() %>%
  id_select(illnesses) %>%
  dplyr::mutate(
    # Calculate row-wise sums for each group
    score_cardiometabolic = rowSums(across(all_of(group_cardiometabolic)), na.rm = TRUE),
    score_neurological    = rowSums(across(all_of(group_neurological)), na.rm = TRUE),
    score_autoimmune      = rowSums(across(all_of(group_autoimmune)), na.rm = TRUE),
    score_respiratory     = rowSums(across(all_of(group_respiratory_atopic)), na.rm = TRUE),
    score_oncology        = rowSums(across(all_of(group_oncology)), na.rm = TRUE),
    score_musculoskeletal = rowSums(across(all_of(group_musculoskeletal_pain)), na.rm = TRUE)
  ) %>%
  id_select(starts_with("score_"))

cache("grouped_illnesses")

lab_grouped_illnesses <- c(
  score_cardiometabolic = "Cardiometabolic & endocrine Disorders",
  score_neurological    = "Neurological conditions",
  score_autoimmune      = "Autoimmune & inflammatory Diseases",
  score_respiratory     = "Respiratory & atopic conditions",
  score_oncology        = "Oncological disorders",
  score_musculoskeletal = "Musculoskeletal & pain disorders"
)

cache("lab_grouped_illnesses")

# ==============================================================================
# LONGITUDINAL DATA MUNGING
# ==============================================================================

# Helper to pivot med columns to long format
pivot_meds <- function(df, prefix, value_name) {
  df %>%
    select(-contains("a_different_antidepressant")) %>%
    na_convert() %>%
    pivot_longer(
      cols = starts_with(prefix),
      names_to = "medication",
      values_to = value_name
    ) %>%
    dplyr::mutate(
      medication = str_remove(medication, paste0(prefix, "\\."))
    )
}

# 1. Start Age & Cumulative Count (Ordering)
# ------------------------------------------------------------------------------
# Medication-use long table (used across outcomes)
med_use_long <- prescription_antidepressants_id %>%
  na_convert() %>%
  pivot_meds("prescription", "took_med") %>%
  dplyr::mutate(took_med = coalesce(took_med, 0))

taken_meds_long <- med_use_long %>%
  filter(took_med == 1) %>%
  select(ID, medication)

# We use start age to determine the order of medications.
# For taken medications with missing start age, assign them to the same age as
# the last valid medication age for that participant.
start_age_long <- taken_meds_long %>%
  left_join(
    antidepressants_why_glad_med_id %>%
      pivot_meds("antidepressants_why.started_taking", "start_age") %>%
      mutate(medication = str_remove(medication, "^txt\\.")),
    by = c("ID", "medication")
  ) %>%
  dplyr::mutate(start_age_valid = ifelse(start_age < 5 | start_age > 100, NA, start_age)) %>%
  group_by(ID) %>%
  dplyr::arrange(start_age_valid, medication, .by_group = TRUE) %>%
  dplyr::mutate(
    n_missing_start_age = sum(is.na(start_age_valid)),
    n_valid_start_age = sum(!is.na(start_age_valid)),
    last_start_age = ifelse(n_valid_start_age > 0, max(start_age_valid, na.rm = TRUE), NA_real_),
    base_cumulative_med_count = ifelse(!is.na(start_age_valid), cumsum(!is.na(start_age_valid)), NA_integer_),
    final_cumulative_med_count = ifelse(n_valid_start_age > 0,
      n_valid_start_age + n_missing_start_age,
      n_missing_start_age
    ),
    cumulative_med_count = case_when(
      !is.na(start_age_valid) & base_cumulative_med_count == n_valid_start_age ~ final_cumulative_med_count,
      !is.na(start_age_valid) ~ base_cumulative_med_count,
      TRUE ~ final_cumulative_med_count
    ),
    start_age = ifelse(is.na(start_age_valid) & n_valid_start_age > 0, last_start_age, start_age_valid)
  ) %>%
  ungroup() %>%
  select(ID, medication, start_age, cumulative_med_count)

# 2. Efficacy
# ------------------------------------------------------------------------------
eff_long <- antidepressants_eff_glad_med_id %>%
  pivot_meds("antidepressants_eff.antidepressants_work_doesdid", "effectiveness")

# 3. Side Effects (Count and Specifics)
# ------------------------------------------------------------------------------
# Calculate n_se (count of side effects) per med
se_long_raw <- sideeffects_antidepressants_id %>%
  select(-contains("side_effects_stop_taking")) %>%
  na_convert() %>%
  pivot_longer(
    cols = contains("sideeffects."),
    names_to = c("variable", "medication"),
    names_pattern = "sideeffects\\.(.*)\\.(.*)",
    values_to = "value"
  )

se_long <- se_long_raw %>%
  # Pivot wider to get columns for each side effect type
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  left_join(med_use_long, by = c("ID", "medication")) %>%
  dplyr::mutate(took_med = coalesce(took_med, 0)) %>%
  rowwise() %>%
  dplyr::mutate(
    all_se_missing = all(is.na(c_across(dry_mouth:other))),
    n_se_raw = sum(c_across(dry_mouth:other), na.rm = TRUE),
    n_se = case_when(
      took_med == 1 & all_se_missing ~ 0,
      took_med == 0 & all_se_missing ~ 0,
      TRUE ~ n_se_raw
    )
  ) %>%
  ungroup() %>%
  select(ID, medication, n_se)

# 4. Remission / Improvement Duration
# ------------------------------------------------------------------------------
remission_long <- antidepressants_imprv_glad_med_id %>%
  pivot_meds("antidepressants_imprv.condition_period_time_experience", "remission")

# 5. Benefits (Count and Rating) - STATIC (Not longitudinal per med)
# ------------------------------------------------------------------------------
# These appear to be overall ratings, not per-medication.
ben_static <- antidepressants_ben_glad_med_id %>%
  na_convert() %>%
  id_select(ben_rating = antidepressants_ben.benefits_rate_taking_antidepressantss) %>%
  dplyr::mutate(ben_rating = factor(ben_rating, ordered = TRUE))

n_best_static <- antidepressants_ben_glad_med_id %>%
  na_convert() %>%
  dplyr::mutate(
    n_best = rowSums(dplyr::select(., starts_with("antidepressants_ben.") & where(is.numeric) & !contains("benefits_rate")), na.rm = TRUE)
  ) %>%
  id_select(n_best)

# 6. Intolerance (Stopped due to SE)
# ------------------------------------------------------------------------------
intolerance_long <- sideeffects_antidepressants_id %>%
  select(ID, contains("side_effects_stop_taking")) %>%
  pivot_meds("sideeffects.side_effects_stop_taking", "stopped_due_to_se")

# 7. Join Everything (Longitudinal Part)
# ------------------------------------------------------------------------------
med_data_long <- taken_meds_long %>%
  left_join(start_age_long, by = c("ID", "medication")) %>%
  left_join(eff_long, by = c("ID", "medication")) %>%
  left_join(se_long, by = c("ID", "medication")) %>%
  left_join(remission_long, by = c("ID", "medication")) %>%
  left_join(intolerance_long, by = c("ID", "medication"))

# 8. Join with Static Demographics
# ------------------------------------------------------------------------------
static_dat_list <- list(
  sex_med,
  employment_med,
  signup_bmi_height_weight_med,
  marital_status_med,
  mhd_med,
  disability_illness_bin,
  disabilities_illnesses,
  grouped_illnesses,
  audit_med,
  cidid_recurrence_med,
  gad7_med,
  phq9_med,
  wsas_med,
  n_relatives,
  smoking_pack_year,
  se_rating,
  ben_static,  # Added
  n_best_static, # Added
  avg_start_age # Added
)

static_dat <- reduce(static_dat_list, left_join, by = "ID")

# Final Join
dat_long <- med_data_long %>%
  left_join(static_dat, by = "ID") %>%
  filter(
     Depressive_and_anxiety_disorder_No_depressive_or_anxiety_disorder == "No"
  ) %>%
  select(-Depressive_and_anxiety_disorder_No_depressive_or_anxiety_disorder) %>%
  mutate_if(is.factor, droplevels)

# Format outcomes
dat_long <- dat_long %>%
  dplyr::mutate(
    effectiveness = factor(effectiveness, ordered = TRUE),
    remission = factor(remission, ordered = TRUE),
    ben_rating = factor(ben_rating, ordered = TRUE),
    # Simplify remission if needed (original code cut it)
    # Keeping raw for now, can transform in model step if needed
  )

cache("dat_long")

# Labels
# ------------------------------------------------------------------------------
# Define labels for plotting/tables
labels <- c(
  "Medication Name",
  "Start Age",
  "Cumulative Medication Count",
  "Effectiveness",
  "Number of Side Effects",
  "Remission",
  "Stopped due to Side Effects",
  "Sex (female)",
  "Doing unpaid or voluntary work v.s. In paid employment or self-employed",
  "Full or part-time student v.s In paid employment or self-employed",
  "Looking after home and/or family v.s In paid employment or self-employed",
  "Retired v.s In paid employment",
  "Unable to work because of sickness or disability v.s In paid employment or self-employed",
  "Unemployed v.s. In paid employment or self-employed",
  "BMI (kg/m^2)",
  "In relationship v.s Not in relationship",
  "Married v.s Not in relationship",
  "Eating disorders",
  "ADHD",
  "Obsessive compulsive disorders",
  "Personality disorders",
  "Autism spectrum disorders",
  "Number of comorbidities",
  "Only anxiety disorder v.s Depressive and anxiety disorder",
  "Only depressive disorder v.s Depressive and anxiety disorder",
  "Only bipolar disorder v.s No psychotic or bipolar disorder",
  "Only psychotic disorder v.s No psychotic or bipolar disorder",
  "Psychotic and bipolar disorder v.s No psychotic or bipolar disorder",
  "Disability or illness",
  lab_illnesses,
  lab_grouped_illnesses,
  "Alcohol use disorder",
  "Number of depressive disorder episodes",
  "Current anxiety",
  "Current depression",
  "Work and social impairment",
  "Number of relatives with psychiatric disorders",
  "Pack years of cigarettes smoked",
  "Benefit rating",
  "Number of best aspects",
  "Average starting age/10",
  "Side effect severity rating"
)

# Map labels to column names
# We do this manually or by name matching.
# Let's create a named vector.
label_names <- c(
  "medication",
  "start_age",
  "cumulative_med_count",
  "effectiveness",
  "n_se",
  "remission",
  "stopped_due_to_se",
  "sex",
  "In_paid_employment_or_self_employed_Doing_unpaid_or_voluntary_work",
  "In_paid_employment_or_self_employed_Full_or_part_time_student",
  "In_paid_employment_or_self_employed_Looking_after_home_and_or_family",
  "In_paid_employment_or_self_employed_Retired",
  "In_paid_employment_or_self_employed_Unable_to_work_because_of_sickness_or_disability",
  "In_paid_employment_or_self_employed_Unemployed",
  "bmi",
  "Not_in_relationship_In_relationship",
  "Not_in_relationship_Married",
  "eating_disorders_numeric",
  "mhd_addadhd_numeric",
  "obsessive_compulsive_disorders_numeric",
  "mhd_personality_disorder_numeric",
  "autism_spectrum_disorder_numeric",
  "comorbidity_total_count_numeric",
  "Depressive_and_anxiety_disorder_Only_anxiety_disorder",
  "Depressive_and_anxiety_disorder_Only_depressive_disorder",
  "No_psychotic_or_bipolar_disorder_Only_bipolar_disorder",
  "No_psychotic_or_bipolar_disorder_Only_psychotic_disorder",
  "No_psychotic_or_bipolar_disorder_Psychotic_and_bipolar_disorder",
  "disable_bin",
  illnesses,
  names(lab_grouped_illnesses),
  "audit",
  "cidid_recurrence",
  "gad7",
  "phq9",
  "wsas",
  "n_relatives",
  "pack_year",
  "ben_rating",
  "n_best",
  "avg_start_age",
  "se_rating"
)

labels <- setNames(labels, label_names)
cache("labels")
