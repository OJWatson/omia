# omia

`omia` is an R package for epidemiological/statistical outbreak workflows, including:

- incidence data preparation
- time-varying Rt estimation (EpiEstim wrapper)
- renewal-equation expected incidence calculations
- Poisson process simulation
- Poisson growth model fitting with doubling time
- uncertainty-propagating short-term forecasts from growth models
- rolling-origin forecast evaluation (MAE/RMSE/coverage/WIS/CRPS)
- epichains-based branching-process simulation for chain-aware uncertainty
- model comparison using AIC/BIC (and optional LOOIC)
- epidemic and Rt visualization
- outbreak consensus summaries

## Installation

```r
# install.packages("remotes")
remotes::install_local(".")
```

## Quick example

```r
library(omia)

data(cat_ba_week1)

inc <- prep_incidence_data(cat_ba_week1)
growth <- fit_poisson_growth(inc)
summary_tbl <- outbreak_consensus_summary(
  incidence_data = inc,
  rt_estimates = tibble::tibble(),
  growth_fit = growth
)

summary_tbl
```

## Optional dependencies

Some functions use optional packages when available:

- `estimate_rt_epiestim()` requires `EpiEstim`.
- `compare_incidence_models(include_looic = TRUE)` can use `loo`.
