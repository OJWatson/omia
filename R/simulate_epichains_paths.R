#' Simulate uncertainty-aware transmission chains with epichains
#'
#' Simulates short-horizon incidence paths using `epichains::simulate_chains()`,
#' allowing `R_t` uncertainty via scalar or vector input.
#'
#' @param rt Reproduction number input. Scalar, or vector of draws to sample
#'   from across simulations.
#' @param n_sims Number of simulations.
#' @param horizon Number of forecast days (including day 0 in raw output).
#' @param offspring_dist Offspring distribution (`"poisson"` or `"nbinom"`).
#' @param dispersion Negative-binomial size parameter when
#'   `offspring_dist = "nbinom"`.
#' @param seed Optional random seed.
#'
#' @return A list with:
#'   * `simulations`: row-level simulated incidences by `sim` and `day_ahead`;
#'   * `summary`: day-level mean/median/5th/95th quantiles.
#' @export
simulate_epichains_paths <- function(
    rt,
    n_sims = 300L,
    horizon = 7L,
    offspring_dist = c("poisson", "nbinom"),
    dispersion = 10,
    seed = NULL
) {
  offspring_dist <- rlang::arg_match(offspring_dist)

  if (!is.numeric(rt) || length(rt) < 1L || any(!is.finite(rt)) || any(rt < 0)) {
    rlang::abort("`rt` must be numeric, finite, and non-negative.")
  }

  if (!is.numeric(n_sims) || length(n_sims) != 1L ||
      n_sims < 1 || n_sims != as.integer(n_sims)) {
    rlang::abort("`n_sims` must be a positive integer.")
  }

  if (!is.numeric(horizon) || length(horizon) != 1L ||
      horizon < 1 || horizon != as.integer(horizon)) {
    rlang::abort("`horizon` must be a positive integer.")
  }

  if (!is.numeric(dispersion) || length(dispersion) != 1L ||
      !is.finite(dispersion) || dispersion <= 0) {
    rlang::abort("`dispersion` must be a single positive finite numeric value.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  rt_sims <- if (length(rt) == 1L) rep(rt, n_sims) else sample(rt, size = n_sims, replace = TRUE)

  sim_tbl <- purrr::map_dfr(seq_len(n_sims), function(i) {
    this_rt <- rt_sims[[i]]

    chain <- if (offspring_dist == "poisson") {
      epichains::simulate_chains(
        n_chains = 1,
        statistic = "size",
        offspring_dist = stats::rpois,
        lambda = this_rt,
        generation_time = function(n) rep(1, n),
        t0 = 0,
        tf = horizon
      )
    } else {
      epichains::simulate_chains(
        n_chains = 1,
        statistic = "size",
        offspring_dist = stats::rnbinom,
        mu = this_rt,
        size = dispersion,
        generation_time = function(n) rep(1, n),
        t0 = 0,
        tf = horizon
      )
    }

    counts <- as.data.frame(chain) |>
      dplyr::count(.data$time, name = "incidence")

    names(counts)[names(counts) == "time"] <- "day_ahead"

    counts <- tidyr::complete(
      counts,
      day_ahead = 0:horizon,
      fill = list(incidence = 0L)
    ) |>
      dplyr::arrange(.data$day_ahead)

    counts$sim <- i
    counts$rt <- this_rt
    counts[, c("sim", "day_ahead", "rt", "incidence")]
  })

  summary_tbl <- sim_tbl |>
    dplyr::group_by(.data$day_ahead) |>
    dplyr::summarise(
      mean_incidence = mean(.data$incidence),
      median_incidence = stats::median(.data$incidence),
      q05 = stats::quantile(.data$incidence, probs = 0.05, names = FALSE),
      q95 = stats::quantile(.data$incidence, probs = 0.95, names = FALSE),
      .groups = "drop"
    )

  list(
    simulations = sim_tbl,
    summary = summary_tbl
  )
}
