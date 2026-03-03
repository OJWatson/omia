validate_required_cols <- function(data, cols, arg = "data") {
  if (!is.data.frame(data)) {
    rlang::abort(paste0("`", arg, "` must be a data frame."))
  }

  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns in `", arg, "`: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}

as_date_safe <- function(x, col_name) {
  if (inherits(x, "Date")) {
    return(x)
  }

  out <- as.Date(x)
  if (any(is.na(out) & !is.na(x))) {
    rlang::abort(
      paste0("Column `", col_name, "` could not be fully converted to Date.")
    )
  }

  out
}

validate_nonnegative_numeric <- function(x, name) {
  if (!is.numeric(x)) {
    rlang::abort(paste0("`", name, "` must be numeric."))
  }
  if (any(!is.finite(x), na.rm = TRUE)) {
    rlang::abort(paste0("`", name, "` must contain finite values."))
  }
  if (any(x < 0, na.rm = TRUE)) {
    rlang::abort(paste0("`", name, "` must be non-negative."))
  }
}

normalize_serial_interval <- function(serial_interval) {
  validate_nonnegative_numeric(serial_interval, "serial_interval")

  if (length(serial_interval) == 0L) {
    rlang::abort("`serial_interval` must have positive length.")
  }

  si_sum <- sum(serial_interval)
  if (si_sum <= 0) {
    rlang::abort("`serial_interval` must sum to a positive value.")
  }

  serial_interval / si_sum
}

build_rt_matrix <- function(rt, n_sims, horizon) {
  if (is.matrix(rt)) {
    if (ncol(rt) != horizon) {
      rlang::abort("When `rt` is a matrix, it must have `horizon` columns.")
    }

    out <- apply(rt, 2, as.numeric)
    if (is.null(dim(out))) {
      out <- matrix(out, nrow = 1)
    }
  } else {
    if (!is.numeric(rt)) {
      rlang::abort("`rt` must be numeric (scalar, vector, or matrix).")
    }

    if (!is.numeric(n_sims) || length(n_sims) != 1L ||
        n_sims < 1 || n_sims != as.integer(n_sims)) {
      rlang::abort("`n_sims` must be a positive integer.")
    }

    if (length(rt) == 1L) {
      out <- matrix(rt, nrow = n_sims, ncol = horizon)
    } else if (length(rt) == horizon) {
      out <- matrix(rt, nrow = n_sims, ncol = horizon, byrow = TRUE)
    } else {
      rlang::abort("`rt` must have length 1, length `horizon`, or be an n_sims x horizon matrix.")
    }
  }

  if (any(!is.finite(out)) || any(out < 0)) {
    rlang::abort("`rt` values must be finite and non-negative.")
  }

  out
}

coerce_forecast_draws <- function(forecast_draws) {
  if (is.matrix(forecast_draws)) {
    out <- tibble::tibble(
      sim = rep(seq_len(nrow(forecast_draws)), times = ncol(forecast_draws)),
      day = rep(seq_len(ncol(forecast_draws)), each = nrow(forecast_draws)),
      incidence = as.numeric(forecast_draws)
    )
  } else if (is.data.frame(forecast_draws)) {
    validate_required_cols(forecast_draws, c("sim", "day", "incidence"), arg = "forecast_draws")
    out <- forecast_draws |>
      dplyr::transmute(
        sim = as.integer(.data$sim),
        day = as.integer(.data$day),
        incidence = as.numeric(.data$incidence)
      )
  } else {
    rlang::abort("`forecast_draws` must be a matrix or data frame/tibble.")
  }

  if (any(!is.finite(out$incidence)) || any(out$incidence < 0, na.rm = TRUE)) {
    rlang::abort("Forecast draws must be finite non-negative values.")
  }

  if (any(is.na(out$sim)) || any(is.na(out$day))) {
    rlang::abort("Forecast draw indices (`sim`, `day`) must be non-missing integers.")
  }

  out |>
    dplyr::arrange(.data$day, .data$sim)
}

interval_score <- function(y, lower, upper, alpha) {
  (upper - lower) +
    (2 / alpha) * (lower - y) * (y < lower) +
    (2 / alpha) * (y - upper) * (y > upper)
}

weighted_interval_score <- function(y, draws, levels) {
  levels <- sort(unique(levels))

  med <- stats::median(draws)
  wis_num <- 0.5 * abs(y - med)

  for (level in levels) {
    alpha <- 1 - level
    lower <- as.numeric(stats::quantile(draws, probs = alpha / 2, names = FALSE, type = 8))
    upper <- as.numeric(stats::quantile(draws, probs = 1 - alpha / 2, names = FALSE, type = 8))
    wis_num <- wis_num + (alpha / 2) * interval_score(y, lower, upper, alpha)
  }

  wis_num / (length(levels) + 0.5)
}

sample_crps <- function(y, draws) {
  x <- sort(as.numeric(draws))
  n <- length(x)

  if (n < 2L) {
    rlang::abort("At least two forecast draws are required to compute CRPS.")
  }

  term1 <- mean(abs(x - y))
  idx <- seq_len(n)
  e_abs_xx <- (2 / (n^2)) * sum((2 * idx - n - 1) * x)
  term1 - 0.5 * e_abs_xx
}
