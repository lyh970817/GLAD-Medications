# Freshness of the cached model objects.
#
# The model objects src/02.regression.r caches with ProjectTemplate's
# `cache("name")` (glmm_models, ..., sef_models, ...) are restored by name on
# every reload.project() (`cache_loading: TRUE`). Nothing in that mechanism
# knows which data they were fitted on, so after a change to the frames (for
# example the 2026-11-12 switch of the main side-effect coding) src/03-06
# would read results fitted on the old data as if they were current.
#
# So src/02 stamps each object with the digest of the analysis frame it was
# assembled from (`dat_long` for the longitudinal families, `dat` for the
# per-participant ones), and every reader checks the stamp against the frame
# it has just rebuilt: objects_present() (lib/08.table_helpers.r) treats an
# object whose stamp is missing or different as absent, so src/04 and src/05
# skip what needs it and src/06 stops; src/03 calls stop_unless_current().
# Re-running src/02 restamps everything; that run is cheap when nothing needs
# refitting, because the GLMM sets come from cache/glmm_fits/ (keyed by their
# model frames) and the per-participant families from fit_or_cached()'s input
# digest (src/02).
MODEL_SOURCE_FRAME <- c(
  glmm_models = "dat_long",
  glmm_models_cov = "dat_long",
  glmm_models_cov_famhist = "dat_long",
  glmm_models_cov_class = "dat_long",
  glmm_models_cov_cohort = "dat_long",
  glmm_models_cov_drug = "dat_long",
  glmm_models_cov_zero = "dat_long",
  sef_models = "dat",
  eff_models = "dat",
  sef_models_compete = "dat",
  eff_models_compete = "dat",
  sef_models_cohort = "dat",
  eff_models_cohort = "dat"
)

# Digest of one analysis frame as it is in the global environment.
frame_digest <- function(frame) {
  digest::digest(get(frame, envir = globalenv(), inherits = FALSE))
}

# `x` with the stamp of the frame it was assembled from.
stamp_source_frame <- function(x, frame) {
  attr(x, "source_frame") <- frame
  attr(x, "source_digest") <- frame_digest(frame)
  attr(x, "stamped_at") <- Sys.time()
  x
}

# "current", "missing", "unstamped" (cached before stamps existed) or "stale"
# (stamped from a different frame) for each of `names`. Objects that are not
# model families are "current" when they exist.
model_object_status <- function(names) {
  vapply(names, function(name) {
    if (!exists(name, envir = globalenv(), inherits = FALSE)) {
      return("missing")
    }
    frame <- MODEL_SOURCE_FRAME[name]
    if (is.na(frame)) {
      return("current")
    }
    x <- get(name, envir = globalenv(), inherits = FALSE)
    stamp <- attr(x, "source_digest")
    if (is.null(stamp)) {
      return("unstamped")
    }
    if (!exists(frame, envir = globalenv(), inherits = FALSE) || !identical(stamp, frame_digest(frame))) {
      return("stale")
    }
    "current"
  }, character(1))
}

# Reports each object that is in cache/ but cannot be used, once per session.
report_unusable_models <- function(status) {
  seen <- getOption("glad.unusable_models_reported", character())
  bad <- status[status %in% c("unstamped", "stale") & !names(status) %in% seen]
  for (name in names(bad)) {
    message(
      "!! cached `", name, "` ignored: ",
      if (bad[[name]] == "unstamped") {
        "it was cached before src/02.regression.r stamped its source frame"
      } else {
        paste0("it was fitted on a different `", MODEL_SOURCE_FRAME[[name]], "` than the current one")
      },
      "; re-run src/02.regression.r"
    )
  }
  options(glad.unusable_models_reported = c(seen, names(bad)))
  invisible(bad)
}

# For scripts that need every one of `names` and cannot skip (src/03).
stop_unless_current <- function(names) {
  status <- model_object_status(names)
  report_unusable_models(status)
  if (any(status != "current")) {
    stop(
      "Needs current ", paste0("`", names(status)[status != "current"], "`", collapse = ", "),
      " from src/02.regression.r (", paste(unique(status[status != "current"]), collapse = ", "), ")",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
