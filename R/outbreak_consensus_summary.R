#' Create outbreak consensus summary
#'
#' Summarizes key outbreak indicators from incidence, Rt estimates,
#' and growth model output.
#'
#' @param incidence_data Data frame with incidence time series.
#' @param rt_estimates Rt estimates tibble (e.g. from [estimate_rt_epiestim()]).
#' @param growth_fit Output list from [fit_poisson_growth()].
#' @param model_comparison Optional output from [compare_incidence_models()].
#' @param date_col Name of date column in `incidence_data`.
#' @param count_col Name of incidence column in `incidence_data`.
#'
#' @return A one-row tibble with high-level outbreak metrics.
#' @export
outbreak_consensus_summary <- function(
    incidence_data,
    rt_estimates,
    growth_fit,
    model_comparison = NULL,
    date_col = "date",
    count_col = "incidence"
) {
  incid_tbl <- prep_incidence_data(
    data = incidence_data,
    date_col = date_col,
    count_col = count_col,
    complete_dates = TRUE
  )

  if (!is.list(growth_fit) || !all(c("estimates", "model") %in% names(growth_fit))) {
    rlang::abort("`growth_fit` must be output from `fit_poisson_growth()`." )
  }

  growth_est <- growth_fit$estimates
  validate_required_cols(growth_est, c("growth_rate", "doubling_time"), arg = "growth_fit$estimates")

  latest_rt <- NA_real_
  if (is.data.frame(rt_estimates) && nrow(rt_estimates) > 0L && "mean_r" %in% names(rt_estimates)) {
    latest_rt <- rt_estimates$mean_r[[nrow(rt_estimates)]]
  }

  peak_idx <- which.max(incid_tbl$incidence)
  peak_date <- incid_tbl$date[[peak_idx]]
  peak_incidence <- incid_tbl$incidence[[peak_idx]]

  growth_rate <- growth_est$growth_rate[[1]]
  trend <- dplyr::case_when(
    is.na(growth_rate) ~ "undetermined",
    growth_rate > 0 ~ "growing",
    growth_rate < 0 ~ "declining",
    TRUE ~ "stable"
  )

  best_model <- NA_character_
  if (!is.null(model_comparison) && is.data.frame(model_comparison) && nrow(model_comparison) > 0L) {
    if (all(c("model", "aic") %in% names(model_comparison))) {
      best_model <- model_comparison |>
        dplyr::filter(!is.na(.data$aic)) |>
        dplyr::slice_min(.data$aic, n = 1, with_ties = FALSE) |>
        dplyr::pull(.data$model)
      if (length(best_model) == 0L) {
        best_model <- NA_character_
      }
    }
  }

  tibble::tibble(
    start_date = min(incid_tbl$date),
    end_date = max(incid_tbl$date),
    days_observed = nrow(incid_tbl),
    total_incidence = sum(incid_tbl$incidence),
    peak_date = peak_date,
    peak_incidence = peak_incidence,
    latest_rt = latest_rt,
    growth_rate = growth_rate,
    doubling_time = growth_est$doubling_time[[1]],
    trend = trend,
    best_model = best_model
  )
}
