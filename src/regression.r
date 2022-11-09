setwd("~/Yandex.Disk/Projects/Research/TNG/GLAD/GLAD_Medications_New")
library(ProjectTemplate)
library(tidyverse)
library(rlist)
reload.project()

# Why so many missing for cidia?
# Need cidia script

fit_model <- function(deps, indeps, covs1 = NULL, condition_fun = NULL, condition_var = NULL, dat) {
  map(deps, function(d) {
    if (is.numeric(dat[[d]])) {
      fit <- fit_hurdle
    } else {
      fit <- fit_prop
    }

    condition_fun <- list(function(x) {
      list(filter(x, mpg > mean(mpg)), filter(x, mpg < mean(mpg)))
    }, function(x) {
      list(filter(x, cyl > mean(cyl)), filter(x, cyl < mean(cyl)))
    })

    d <- list(as_tibble(mtcars))
    for (f in condition_fun) {
      recurr <- function(x) {
        map(
          x,
          function(x) {
            if (is.list(x) & !is.data.frame(x)) {
              recurr(x)
            } else {
              f(x)
            }
          }
        )
      }
      d <- recurr(d)
    }

    # unlist(d)

    # Check for converging
    # table(dat[["mhd_schizophrenia_numeric"]])
    # table(dat[["mhd_schizoaffective_numeric"]])
    # mat <- dat %>%
    #   na_omit_dat(d, indeps) %>%
    #   .[colnames(.) %in% indeps] %>%
    #   map_df(as.numeric) %>%
    #   as.matrix() %>%
    #   Matrix::rankMatrix()

    indeps <- indeps[indeps %in% colnames(dat)]

    m <- format_model(fit(dat, d, indeps))

    return(m)
  })
}




# Analysis 1: Side effect severity and intolerance
# =====================================
dat_1 <- dat %>%
  select(-ID) %>%
  mutate_at(
    vars(
      -mean_n_se,
      -intolerance,
      -se_rating,
      -depression_and_anxiety,
      -bipolar_and_schizophrenia
    ),
    as.numeric
  )

dat_1p <- dat_1 %>%
  filter(
    bipolar_and_schizophrenia != "Psychotic and bipolar disorder" &
      bipolar_and_schizophrenia != "Only bipolar disorder" &
      depression_and_anxiety != "Only anxiety disorder"
  )


indeps <- c(
  "n_relatives",
  "bmi",
  "pack_year",
  "marital",
  "employment",
  "bipolar_and_schizophrenia",
  "eating_disorders_numeric",
  "mhd.addadhd_numeric",
  "obsessive_compulsive_disorders_numeric",
  "mhd.personality_disorder_numeric",
  "autism_spectrum_disorder_numeric",
  "comorbidity_total_count_numeric",
  "audit",
  "time",
  "wsas",
  "phq9"
  # "gad7"
)


sef_deps <- c("mean_n_se", "se_rating", "intolerance")

covs <- c("sex", "score_phq", "score_gad", "score_pad")
indeps_now <- c("score_phq", "score_gad", "score_wsas", "score_pad", "age")

m1_1 <- fit_model(deps = sef_deps, indeps = indeps, covs = covs, dat = dat_1)
only_sig(m1_1)

m1_2 <- fit_model(sef_deps, indeps2, covs, dat_1)

m1_1p <- fit_model(sef_deps, (indeps), covs1, dat_1p)
m1_2p <- fit_model(sef_deps, indeps2, covs1, dat_1p)

dat_1_now <- dat_1[last_treatment_now, ]
dat_1p_now <- dat_1 %>%
  filter(mhdc_bip != 1 & mhdc_depanx != 2)

m1_3 <- fit_model(sef_deps, c(indeps, indeps_now), "sex", dat_1_now)
m1_4 <- fit_model(sef_deps, c(indeps2, indeps_now), "sex", dat_1_now)

m1_3p <- fit_model(
  sef_deps, c((indeps), indeps_now),
  "sex", dat_1p_now
)
m1_4p <- fit_model(sef_deps, c(indeps2, indeps_now), "sex", dat_1p_now)

# No empolyment

m1_1_noem <- fit_model(sef_deps, indeps_noem, covs, dat_1)
m1_2_noem <- fit_model(sef_deps, indeps2_noem, covs, dat_1)

m1_1p_noem <- fit_model(sef_deps, (indeps_noem), covs1, dat_1p)
m1_2p_noem <- fit_model(sef_deps, indeps2_noem, covs1, dat_1p)

m1_3_noem <- fit_model(sef_deps, c(indeps, indeps_now), "sex", dat_1_now)
m1_4_noem <- fit_model(sef_deps, c(indeps2, indeps_now), "sex", dat_1_now)

m1_3p_noem <- fit_model(
  sef_deps, c((indeps), indeps_now),
  "sex", dat_1p_now
)
m1_4p_noem <- fit_model(sef_deps, c(indeps2, indeps_now), "sex", dat_1p_now)

# Analysis 2 Associations with effectiveness
# =====================================

dat_2 <- dat %>%
  select(-ID) %>%
  mutate_at(
    vars(
      -remission,
      -ben_rating,
      -n_best,
      -first_imprv,
      -mean_eff,
      -depression_and_anxiety,
      -bipolar_and_schizophrenia
    ),
    as.numeric
  )

indeps_eff <- c(
  sef_deps,
  "n_relatives",
  "bmi",
  "pack_year",
  "marital",
  "employment",
  "bipolar_and_schizophrenia",
  "eating_disorders_numeric",
  "mhd.addadhd_numeric",
  "obsessive_compulsive_disorders_numeric",
  "mhd.personality_disorder_numeric",
  "autism_spectrum_disorder_numeric",
  "comorbidity_total_count_numeric",
  "audit",
  "time"
)


eff_deps <- c(
  "remission",
  "ben_rating",
  "n_best",
  "first_imprv",
  "mean_eff"
)

m2_1 <- fit_model(deps = eff_deps, indeps = indeps_eff, covs = covs, dat = dat_2)
only_sig(multi_adjust(m2_1))


dat_2_now <- dat_2[last_treatment_now, ]
dat_2p_now <- dat_2_now %>%
  filter(mhdc_bip != 1 & mhdc_depanx != 2)

m2_3 <- fit_model(eff_deps, c(indeps_eff, indeps_now), "sex", dat_2_now)
m2_4 <- fit_model(eff_deps, c(indeps2_eff, indeps_now), "sex", dat_2_now)

m2_3p <- fit_model(
  eff_deps, c((indeps_eff), indeps_now),
  "sex", dat_2p_now
)
m2_4p <- fit_model(eff_deps, c(indeps2_eff, indeps_now), "sex", dat_2p_now)

# No empolyment

m2_1p_noem <- fit_model(eff_deps, (indeps_eff_noem), covs, dat_2p)
m2_2p_noem <- fit_model(eff_deps, indeps2_eff_noem, covs, dat_2p)

m2_1_noem <- fit_model(eff_deps, indeps_eff_noem, covs, dat_2)
m2_2_noem <- fit_model(eff_deps, indeps2_eff_noem, covs, dat_2)

m2_3_noem <- fit_model(
  eff_deps, c(indeps_eff_noem, indeps_now),
  "sex", dat_2_now
)
m2_4_noem <- fit_model(
  eff_deps, c(indeps2_eff_noem, indeps_now),
  "sex", dat_2_now
)

m2_3p_noem <- fit_model(
  eff_deps, c((indeps_eff_noem), indeps_now),
  "sex", dat_2p_now
)
m2_4p_noem <- fit_model(
  eff_deps, c(indeps2_eff_noem, indeps_now),
  "sex", dat_2p_now
)

for (object in ls() %>% grep("m[12]_", ., v = T)) cache(object)


# Correlation matrix between independent variables
# =====================================

cor_mat_indeps <- polycor::hetcor(as.data.frame(dat[c(
  union(indeps, indeps_eff),
  indeps_now, "sex"
)]), use = "pairwise.complete.obs", std.err = F)$correlations

cor_names <- recode(
  names(dat[c(union(indeps, indeps_eff), indeps_now, "sex")]),
  !!!labels
)
dimnames(cor_mat_indeps) <- list(cor_names, cor_names)
cache("cor_mat_indeps")

cor_plot_indeps <- ggcorrplot(cor_mat_indeps,
  type = "lower",
  lab = TRUE,
  title = "Correlations between indepedent variables in the dataset"
)
cache("cor_plot_indeps")
