#' Compute expected incidence under a renewal equation
#'
#' Calculates expected incidence from historical incidence, a time-varying
#' reproduction number, and a serial interval distribution.
#'
#' @param incidence Numeric vector of observed incidence.
#' @param rt Numeric vector of reproduction numbers of the same length as
#'   `incidence` or length 1.
#' @param serial_interval Numeric vector of serial interval probabilities.
#'
#' @return A tibble with day index, infectiousness, `rt`, and
#'   `expected_incidence`.
#' @export
renewal_expected_incidence <- function(incidence, rt, serial_interval) {
  validate_nonnegative_numeric(incidence, "incidence")
  validate_nonnegative_numeric(serial_interval, "serial_interval")

  n <- length(incidence)
  if (length(rt) == 1L) {
    rt <- rep(rt, n)
  }
  if (length(rt) != n) {
    rlang::abort("`rt` must have length 1 or the same length as `incidence`.")
  }
  validate_nonnegative_numeric(rt, "rt")

  if (sum(serial_interval) <= 0) {
    rlang::abort("`serial_interval` must sum to a positive value.")
  }
  w <- serial_interval / sum(serial_interval)

  lambda <- numeric(n)
  for (t in seq_len(n)) {
    lags <- seq_len(min(t - 1L, length(w)))
    if (length(lags) > 0) {
      lambda[t] <- sum(incidence[t - lags] * w[lags])
    } else {
      lambda[t] <- 0
    }
  }

  expected <- rt * lambda

  tibble::tibble(
    day = seq_len(n),
    infectiousness = lambda,
    rt = rt,
    expected_incidence = expected
  )
}
