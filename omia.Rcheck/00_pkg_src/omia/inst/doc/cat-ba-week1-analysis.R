## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(omia)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
data(cat_ba_week1)
inc <- prep_incidence_data(cat_ba_week1)
inc

## ----fig.height=4, fig.width=7------------------------------------------------
plot_epicurve(inc)

## -----------------------------------------------------------------------------
growth <- fit_poisson_growth(inc)
growth$estimates

## ----fig.height=4, fig.width=7------------------------------------------------
growth$fitted |>
  ggplot(aes(date, incidence)) +
  geom_col(fill = "grey80") +
  geom_line(aes(y = fitted), color = "#1f77b4", linewidth = 1) +
  labs(
    title = "Observed incidence vs Poisson growth fit",
    x = NULL,
    y = "Daily cases"
  )

## -----------------------------------------------------------------------------
si <- c(0.1, 0.2, 0.3, 0.25, 0.15)
renewal_tbl <- renewal_expected_incidence(
  incidence = inc$incidence,
  rt = 1.2,
  serial_interval = si
)
head(renewal_tbl)

## -----------------------------------------------------------------------------
sims <- simulate_poisson_process(
  expected_incidence = renewal_tbl$expected_incidence,
  n_sims = 100,
  seed = 42
)

sim_summary <- sims |>
  group_by(day) |>
  summarise(
    mean_sim = mean(incidence),
    q05 = quantile(incidence, 0.05),
    q95 = quantile(incidence, 0.95),
    .groups = "drop"
  )

head(sim_summary)

## -----------------------------------------------------------------------------
model_df <- inc |> mutate(day = as.integer(date - min(date)) + 1)
m_growth <- stats::glm(incidence ~ day, family = poisson(), data = model_df)
m_null <- stats::glm(incidence ~ 1, family = poisson(), data = model_df)

compare_incidence_models(list(growth = m_growth, null = m_null))

## -----------------------------------------------------------------------------
summary_tbl <- outbreak_consensus_summary(
  incidence_data = inc,
  rt_estimates = tibble::tibble(),
  growth_fit = growth,
  model_comparison = compare_incidence_models(list(growth = m_growth, null = m_null))
)
summary_tbl

## ----eval=FALSE---------------------------------------------------------------
# rt <- estimate_rt_epiestim(inc)
# plot_rt_estimates(rt)

