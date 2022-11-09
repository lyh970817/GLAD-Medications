kable_prop <- function(model) {
  colnames(model) <- make.unique(colnames(model))
  levels <- str_extract(colnames(model), "[0-9]+") %>%
    unique() %>%
    subset(!is.na(.))
  dep <- labels[attr(model, "dep")]
  n <- attr(model, "n")
  vars <- model["var"]

  model <- map(levels, function(i) {
    model[grep(paste0("_", i), colnames(model))]
  }) %>%
    bind_cols()

  model <- bind_cols(vars, model)

  # Find which row has NA and hence is ordinal effect
  # row_group <- which(apply(tab, 1, function(x) any(is.na(x))))[1]

  # headers <- map_chr(
  #   levels,
  #   ~ paste("Higher than", .x, "(vs. lower than or equal to", .x, ")")
  # )

  levels <- str_extract(colnames(model), "[0-9]+") %>%
    unique() %>%
    subset(!is.na(.))

  pos <- map_dbl(levels, function(l) {
    length(grep(paste0("_", l), colnames(model)))
  })

  p_headers <- map(n, function(x) {
    c("OR (95%CI)", paste0("p (n = ", x, ")"))
  })

  n_pairs <- pos / 2

  # Later sample must contain the previous samples
  # so if only one model at some level, it must be the first sample

  p_headers_all <- map(n_pairs, function(n) {
    unlist(p_headers[seq(n)])
  }) %>%
    unlist()

  pos <- c(1, pos)

  names(pos) <- c(" ", " ")

  model %>%
    mutate_at(
      vars(contains("p")),
      ~ ifelse(. < 0.05, cell_spec(scales::pvalue(.), "html", bold = T), round(., digits = 2))
    ) %>%
    setNames(c(dep, p_headers_all)) %>%
    kable(format = "html", escape = F) %>%
    # pack_rows("Nominal effects", 1, row_group - 1) %>%
    # pack_rows("Ordinal effects", row_group, nrow(tab)) %>%
    add_header_above(pos) %>%
    kable_styling(full_width = T)
}

kable_hurd <- function(model) {
  dep <- labels[attr(model, "dep")]

  n <- attr(model, "n")
  vars <- model["var"]

  model <- map(1:2, function(i) {
    model[grep(paste0("_", i), colnames(model))]
  }) %>%
    bind_cols()

  model <- bind_cols(vars, model)

  headers <- c(
    paste(dep, "(Y = 0)"),
    paste(dep, "(Y > 0)")
  )

  levels <- str_extract(colnames(model), "[0-9]+") %>%
    unique() %>%
    subset(!is.na(.))

  pos <- map_dbl(levels, function(l) {
    length(grep(paste0("_", l), colnames(model)))
  })

  pos <- c(1, pos)
  names(pos) <- c(" ", headers)

  p_headers <- map(n, function(x) {
    c("OR (95%CI)", paste0("p (n = ", x, ")"))
  }) %>%
    unlist()

  model %>%
    mutate_at(
      vars(contains("p")),
      ~ ifelse(. < 0.05, cell_spec(scales::pvalue(.), "html", bold = T), round(., digits = 2))
    ) %>%
    setNames(c(
      "", p_headers
    )) %>%
    kable(format = "html", escape = F) %>%
    add_header_above(pos)
}

format_model <- function(model) {
  # Can be changed to be for ordinal model with one shared beta
  stats_gen <- function(tab) {
    tab["esti"] <- paste0(
      tab[["esti"]], " ",
      "(",
      paste(tab[["low"]], tab[["high"]], sep = ", "),
      ")"
    )
    tab <- tab[!colnames(tab) %in% c("low", "high")]
    return(tab)
  }

  tab <- model %>%
    as_tibble() %>%
    setNames(c("Parameters", "esti", "low", "high", "p")) %>%
    dplyr::mutate(time = Parameters) %>%
    filter(!grepl("Intercept|^[0-9]\\|[0-9]$", time)) %>%
    mutate_at(vars(c(esti, low, high)), ~ format_digit(.)) %>%
    stats_gen() %>%
    mutate(var = str_extract(time, "[a-z_]+[0-9]{0,2}")) %>%
    mutate(time = str_extract(time, "^[0-9]")) %>%
    # Give the NA column a number index. length(unique(.)) because it's
    # character
    mutate_at(vars(time), ~ ifelse(is.na(.), length(unique(.)), .)) %>%
    select(-Parameters) %>%

    # NEEDS A MORE REASONABLE METHOD !!!!
    pivot_wider(
      names_from = time, values_from = c(esti, p),
      values_fn = function(x) {
        x[[1]]
      }
    ) %>%
    mutate(var = recode(var, !!!labels)) %>%
    arrange(var)

  seq_levels <- unique(str_extract(colnames(tab)[-1], "[0-9]"))

  e_fst <- paste("esti", min(seq_levels), sep = "_")
  p_fst <- paste("p", min(seq_levels), sep = "_")

  ordered_cols <- map(
    seq_levels,
    function(num) {
      list(paste0(c("esti", "p"), "_", num))
    }
  ) %>%
    unlist()

  tab <- tab[c("var", ordered_cols)]
  tab[is.na(tab[[e_fst]]), e_fst] <- tab[is.na(tab[[e_fst]]), paste("esti", tail(seq_levels, 1), sep = "_")]
  tab[is.na(tab[[p_fst]]), p_fst] <- tab[is.na(tab[[p_fst]]), paste("p", tail(seq_levels, 1), sep = "_")]

  # Unless we fit a model with a three-level factor and it has no ordinal
  # effect (which is impossible) seq_levels will always be larger than 2
  if (length(seq_levels) > 2) {
    tab[paste("esti", tail(seq_levels, 1), sep = "_")] <- NULL
    tab[paste("p", tail(seq_levels, 1), sep = "_")] <- NULL
  }

  attr(tab, "n") <- attr(model, "n")
  attr(tab, "dep") <- attr(model, "dep")

  return(tab)
}

full_join_byvar <- function(m1, m2) {
  full_join(m1, m2, by = "var")
}

kable_hurd_all <- function(models) {
  model <- reduce(models, full_join_byvar) %>%
    arrange(p_1.y)

  dep <- labels[attr(models[[1]], "dep")]

  ns <- map(models, attr, "n")

  vars <- models[[1]]["var"]

  headers <- c(
    paste(dep, "(Y = 0)"),
    paste(dep, "(Y > 0)")
  )

  pos <- c(1, rep(c(rep(2, 2), rep(2, 2), rep(2, 2), rep(2, 2)), 2) %>%
    rep(2))
  names(pos) <- c(" ", rep(rep(headers, 4), 2) %>%
    rep(2))

  pos_meta <- c(1, rep(c(4, 4, 4, 4), 2) %>%
    rep(2))
  names(pos_meta) <- c(" ", rep(c("Diagnoses", "No diagnoses", "Diagnoses (subgroup)", "No diagnoses (subgroup)"), 2) %>%
    rep(2))

  pos_meta2 <- c(1, rep(16, 2) %>%
    rep(2))
  names(pos_meta2) <- c(
    " ",
    c("Depression only", "Depression/anxiety/bipolar") %>%
      rep(., 2)
  )

  pos_meta3 <- c(1, rep(32, 2))

  names(pos_meta3) <- c(
    " ",
    c("Employment", "No empolyment")
  )

  p_headers <- map(ns, function(x) {
    c(
      "OR (95%CI)", paste0("p (n = ", x[1], ")"),
      "OR (95%CI)", paste0("p (n = ", x[2], ")")
    )
  }) %>%
    unlist()

  model %>%
    mutate_at(
      vars(contains("p")),
      ~ ifelse(. < 0.05, cell_spec(scales::pvalue(.), "html", bold = T), round(., digits = 2))
    ) %>%
    setNames(c(
      "", p_headers
    )) %>%
    kable(format = "html", escape = F) %>%
    add_header_above(pos) %>%
    add_header_above(pos_meta) %>%
    add_header_above(pos_meta2) %>%
    add_header_above(pos_meta3)
}

kable_prop_all <- function(models) {
  model <- reduce(models, full_join_byvar) %>%
    arrange(p_1.y)

  dep <- labels[attr(models[[1]], "dep")]

  ns <- map(models, attr, "n")

  vars <- models[[1]]["var"]


  pos_meta <- c(1, rep(c(2, 2, 2, 2), 2) %>%
    rep(2))

  names(pos_meta) <- c(" ", rep(c("Diagnoses", "No diagnoses", "Diagnoses (subgroup)", "No diagnoses (subgroup)"), 2) %>%
    rep(2))

  pos_meta2 <- c(1, rep(8, 2) %>%
    rep(2))
  names(pos_meta2) <- c(
    " ",
    c("Depression only", "Depression/anxiety/bipolar") %>%
      rep(., 2)
  )

  pos_meta3 <- c(1, rep(16, 2))

  names(pos_meta3) <- c(
    " ",
    c("Employment", "No empolyment")
  )

  p_headers <- map(ns, function(x) {
    c(
      "OR (95%CI)", paste0("p (n = ", x[1], ")"),
      "OR (95%CI)", paste0("p (n = ", x, ")")
    )
  }) %>%
    unlist()

  model %>%
    mutate_at(
      vars(contains("p")),
      ~ ifelse(. < 0.05, cell_spec(scales::pvalue(.), "html", bold = T), round(., digits = 2))
    ) %>%
    setNames(c(
      "", p_headers
    )) %>%
    kable(format = "html", escape = F) %>%
    add_header_above(pos_meta) %>%
    add_header_above(pos_meta2) %>%
    add_header_above(pos_meta3)
}
