#' Compare incidence model fits
#'
#' Compares fitted models using AIC/BIC and optionally LOOIC when available.
#'
#' @param models A named list of fitted model objects.
#' @param include_looic If `TRUE`, attempts to compute LOOIC using
#'   `loo::loo()` and model log-likelihood draws when available.
#'
#' @return A tibble with one row per model and comparison metrics.
#' @export
compare_incidence_models <- function(models, include_looic = FALSE) {
  if (!is.list(models) || length(models) == 0L) {
    rlang::abort("`models` must be a non-empty list of fitted models.")
  }

  model_names <- names(models)
  if (is.null(model_names) || any(model_names == "")) {
    model_names <- paste0("model_", seq_along(models))
  }

  has_loo <- isTRUE(include_looic) && rlang::is_installed("loo")

  out <- purrr::imap_dfr(models, function(mod, nm) {
    aic_val <- tryCatch(stats::AIC(mod), error = function(e) NA_real_)
    bic_val <- tryCatch(stats::BIC(mod), error = function(e) NA_real_)

    looic <- NA_real_
    loo_note <- "not requested"

    if (isTRUE(include_looic) && !has_loo) {
      loo_note <- "package `loo` not installed"
    }

    if (has_loo) {
      ll_mat <- tryCatch(
        loo::log_lik(mod),
        error = function(e) NULL
      )

      if (!is.null(ll_mat)) {
        loo_obj <- tryCatch(loo::loo(ll_mat), error = function(e) NULL)
        if (!is.null(loo_obj)) {
          looic <- unname(loo_obj$estimates["looic", "Estimate"])
          loo_note <- "computed"
        } else {
          loo_note <- "loo computation failed"
        }
      } else {
        loo_note <- "log_lik unavailable for model"
      }
    }

    tibble::tibble(
      model = nm,
      aic = as.numeric(aic_val),
      bic = as.numeric(bic_val),
      looic = looic,
      looic_note = loo_note
    )
  })

  out |>
    dplyr::arrange(.data$aic)
}
