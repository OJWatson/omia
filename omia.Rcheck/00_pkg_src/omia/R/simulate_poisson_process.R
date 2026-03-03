#' Simulate incidence from a Poisson process
#'
#' Draws Poisson realizations given expected incidence values.
#'
#' @param expected_incidence Numeric vector of expected incidence values.
#' @param n_sims Number of simulations.
#' @param seed Optional random seed.
#'
#' @return A tibble with simulation id, day index, expected incidence,
#'   and simulated incidence.
#' @export
simulate_poisson_process <- function(expected_incidence, n_sims = 1L, seed = NULL) {
  validate_nonnegative_numeric(expected_incidence, "expected_incidence")

  if (!is.numeric(n_sims) || length(n_sims) != 1L || n_sims < 1 || n_sims != as.integer(n_sims)) {
    rlang::abort("`n_sims` must be a positive integer.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- length(expected_incidence)

  sims <- purrr::map_dfr(seq_len(n_sims), function(i) {
    tibble::tibble(
      sim = i,
      day = seq_len(n),
      expected_incidence = expected_incidence,
      incidence = stats::rpois(n, lambda = expected_incidence)
    )
  })

  sims
}
