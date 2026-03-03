#' Estimate time-varying reproduction number with EpiEstim
#'
#' Wrapper around `EpiEstim::estimate_R()` with input validation and a tidy
#' return format.
#'
#' @param data A data frame with date and incidence columns.
#' @param date_col Name of date column in `data`.
#' @param count_col Name of incidence column in `data`.
#' @param method Method passed to `EpiEstim::estimate_R()`.
#' @param config Optional EpiEstim configuration list. If `NULL`, a default
#'   parametric serial interval is used.
#' @param ... Additional arguments passed to `EpiEstim::estimate_R()`.
#'
#' @return A tibble containing time window indices, dates, and `R_t` estimates.
#' @export
estimate_rt_epiestim <- function(
    data,
    date_col = "date",
    count_col = "incidence",
    method = "parametric_si",
    config = NULL,
    ...
) {
  if (!rlang::is_installed("EpiEstim")) {
    rlang::abort("Package `EpiEstim` is required for `estimate_rt_epiestim()`. Please install it.")
  }

  incid_tbl <- prep_incidence_data(
    data = data,
    date_col = date_col,
    count_col = count_col,
    complete_dates = TRUE
  )

  incid_vec <- round(incid_tbl$incidence)
  validate_nonnegative_numeric(incid_vec, "incidence")

  if (is.null(config)) {
    config <- EpiEstim::make_config(list(mean_si = 4.7, std_si = 2.9))
  }

  rt_obj <- EpiEstim::estimate_R(
    incid = incid_vec,
    method = method,
    config = config,
    ...
  )

  rt_raw <- tibble::as_tibble(rt_obj$R)

  mean_col <- names(rt_raw)[grepl("Mean\\(R\\)", names(rt_raw))][1]
  low_col <- names(rt_raw)[grepl("0\\.025\\(R\\)", names(rt_raw))][1]
  high_col <- names(rt_raw)[grepl("0\\.975\\(R\\)", names(rt_raw))][1]

  if (any(is.na(c(mean_col, low_col, high_col)))) {
    rlang::abort("Could not identify Rt estimate columns in EpiEstim output.")
  }

  std_r_vec <- if ("Std(R)" %in% names(rt_raw)) {
    rt_raw[["Std(R)"]]
  } else {
    rep(NA_real_, nrow(rt_raw))
  }

  out <- rt_raw |>
    dplyr::transmute(
      t_start = .data$t_start,
      t_end = .data$t_end,
      date_start = incid_tbl$date[.data$t_start],
      date_end = incid_tbl$date[.data$t_end],
      mean_r = .data[[mean_col]],
      lower_95 = .data[[low_col]],
      upper_95 = .data[[high_col]],
      std_r = std_r_vec
    )

  tibble::as_tibble(out)
}
