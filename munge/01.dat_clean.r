require(fastDummies)

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
  id_select(education_yrs = `dem.years_schoolplease_include_preschool.txt`)

# Employment
recode_employed <- c(
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
  factor()
#   fct_recode(!!!recode_employed) %>%
#   fct_recode(!!!recode_unempolyed) %>%
#   factor(levels = c("Unemployed", "Employed"))

employment_med <- employment_glad_clean %>%
  na_convert() %>%
  # Missing 'startDate' and 'endDate'
  # na_row_remove() %>%
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
# Hypen will be confused with minus sign in R formulae
colnames(employment_med) <- gsub("-", "_", colnames(employment_med))
colnames(employment_med) <- gsub("/", "_", colnames(employment_med))

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
# sum(is.na(cidia_recurrence_med$cidia_recurrence))
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

# Number of relatives wtih psychiatric disorder
# Needs updating ilovedata script

n_relatives <- fh_mhd_f_glad_dat %>%
  full_join(fh_mhd2_f_glad_dat, by = c("externalDataReference", "startDate", "endDate")) %>%
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
  # na_row_remove() %>%
  rowwise() %>%
  mutate(n_meds = sum(c_across(prescription.citalopram:prescription.vortioxetine), na.rm = TRUE)) %>%
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
  left_join(n_meds, by = "ID") %>%
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
  left_join(n_meds, by = "ID") %>%
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

# Average starting age
avg_start_age <- started_age[-1] %>%
  rowMeans(na.rm = T) %>%
  bind_cols(started_age["ID"], avg_start_age = .) %>%
  mutate(avg_start_age = avg_start_age / 10)

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
  mutate(first_imprv = factor(first_imprv, ordered = TRUE))

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
# illnesses <- c(
#   "dem.metal_implants_numeric", "dem.epilepsy_or_convulsions_numeric",
#   "dem.migraines_numeric", "dem.multiple_sclerosis_numeric",
#   # Rank deficient
#   "dem.parkinsons_disease_numeric",
#   "dem.severe_memory_loss_numeric",
#   # "dem.neurological_none_numeric", "dem.neurological_dont_know_numeric",
#   # "dem.neurological_prefer_not_to_answer_numeric",
#   "dem.hay_fever_numeric",
#   "dem.drug_allergy_numeric", "dem.food_allergy_numeric",
#   "dem.other_allergy_numeric", "dem.osteoporosis_numeric",
#   "dem.osteoarthritis_numeric", "dem.rheumatoid_arthritis_numeric",
#   "dem.other_arthritis_numeric",
#   # "dem.allergy_none_numeric",
#   "dem.asthma_numeric", "dem.emphysema_or_chronic_bronchitis",
#   "dem.heart_attack_or_angina_numeric", "dem.high_blood_cholesterol_numeric",
#   "dem.high_blood_pressure_numeric", "dem.atrial_fibrillation_numeric",
#   "dem.stroke_numeric",
#   "dem.crohns_disease_numeric", "dem.ulcerative_colitis_numeric",
#   "dem.coeliac_disease_numeric", "dem.diabetes_type_1_numeric",
#   "dem.diabetes_type_2_numeric", "dem.pain_due_to_diabetes_numeric",
#   "dem.pain_due_to_virus_numeric",
#   "dem.breast_cancer_numeric",
#   # Rank deficient
#   "dem.lung_cancer_numeric",
#   "dem.stomach_cancer_numeric",
#   "dem.colon_cancer_numeric",
#   "dem.uterus_cancer_numeric",
#   "dem.prostate_cancer_numeric",
#   "dem.psoriasis_numeric",
#   "dem.vitiligo_numeric", "dem.eczema_numeric",
#   "dem.thyroid_disease_numeric"
#   # "dem.listed_previously_told_illness_numeric"
#   # All NA
#   # "dem.brain_tumour_numeric",
#   # "dem.ankylosing_spondylitis_numeric", "dem.hypermobility_numeric",
#   # "dem.pots_numeric", "dem.diabetes_type_1_early_onset_numeric",
#   # "dem.diabetes_type_1_late_onset_numeric",
#   # "dem.diabetes_type_2_late_onset_numeric",
#   # "dem.pcos_numeric"
# )

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
  mutate(
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
      sum(
        c_across(
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
  avg_start_age,
  # years_of_education_med,
  employment_med,
  signup_bmi_height_weight_med,
  marital_status_med,
  mhd_med,
  disability_illness_bin,
  disabilities_illnesses,
  grouped_illnesses,
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

dat <- reduce(dat_list, left_join, by = "ID") %>%
  filter(
    Depressive_and_anxiety_disorder_No_depressive_or_anxiety_disorder ==
      "No"
  ) %>%
  select(-Depressive_and_anxiety_disorder_No_depressive_or_anxiety_disorder) %>%
  # Remove unused levels
  mutate_if(is.factor, droplevels)


labels <- c(
  "Number of antidepressants",
  "Sex (female)",
  "Average starting age/10",
  # "Years of education",
  "Doing unpaid or voluntary work v.s. In paid employment or self-employed",
  "Full or part-time student v.s In paid employment or self-employed",
  "Looking after home and/or family v.s In paid employment or self-employed",
  "None of the above v.s In paid employment or self-employed",
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
  "Number of best aspects",
  "Benefit rating",
  "Side effect severity rating",
  "Mean number of side effects",
  "Treatment discontinuation",
  "Average effectiveness",
  "First improvement duration",
  "Occurence of remission",
  "Total duration on antidepressants"
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
