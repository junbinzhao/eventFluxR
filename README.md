# eventFluxR

Reusable R functions for detecting, modeling and evaluating "hot-moment" flux
events in biogeochemical time series (e.g. N2O flux chamber data). Developed
for the Svanhovd N2O flux experiment (NIBIO), but written generically for any
flux/event dataset with a similar structure.

The package provides four main functions:

- `identify_hot_moments()` — MAD-based or XGBoost-based hot-moment detection,
  with k-threshold sensitivity checks and event grouping/duration.
- `fertilization_index()` — decay-weighted fertilization index (dose x
  exponential decay) plus days-since-last-event.
- `classswap_model()` / `predict()` — "class-swap" modeling: an XGBoost
  classifier splits observations into background vs. hot moment, and separate
  XGBoost regressors are fit to each class.
- `evaluate_flux_model()` — detection rate, false alarm rate, PR-AUC, RMSE,
  MAE, ME (overall and by flux-magnitude class), and upper-tail bias
  diagnostics.

A bundled example dataset, `n2o_sample`, is included (a real 493-row subset
from four chambers spanning two plots and both fertilization levels, over
the 2022 growing season).

## Installation

```r
# install.packages("remotes") # if not already installed
remotes::install_github("junbinzhao/eventFluxR")
```

## Example

```r
library(eventFluxR)
data(n2o_sample)

## 1) Identify hot moments (MAD-based, per chamber) -------------------------
hm <- identify_hot_moments(
  n2o_sample,
  flux_col   = "N2O_flux",
  group_vars = "Chamber",
  k          = 3,
  time_col   = "Timestamp"
)
table(hm$data$.hot_moment)   # flagged observations
head(hm$events)              # discrete hot-moment events with duration

## 2) Fertilization index (dose decay + days since last event) -------------
events <- as.POSIXct(c("2022-05-02", "2022-08-09"), tz = "UTC")
fert <- fertilization_index(
  n2o_sample$Timestamp, events,
  event_doses = c(90, 36), half_life = 14
)
head(fert)

## 3) Class-swap model: classifier + separate regressors --------------------
feats <- c("CO2_flux", "Soil_Moist", "Soil_T_2", "VPD", "fert_ef")

fit <- classswap_model(
  n2o_sample,
  response_col = "N2O_flux",
  feature_cols = feats,
  group_vars   = "Plot",
  label_col    = "y_bin",
  n_trials     = 15,  # increase for real use (e.g. 100+)
  nrounds      = 300,
  early_stopping_rounds = 30,
  verbose      = FALSE
)
pred <- predict(fit, n2o_sample)

## 4) Evaluate model performance ---------------------------------------------
ev <- evaluate_flux_model(
  obs    = n2o_sample$N2O_flux, pred = pred,
  y_true = n2o_sample$y_bin,
  y_prob = predict(fit$classifier, xgboost::xgb.DMatrix(as.matrix(n2o_sample[feats])))
)
ev$detection        # detection rate / false alarm rate across thresholds
ev$regression        # RMSE / MAE / ME, overall and by magnitude class
ev$upper_tail_bias   # bias in the upper tail of the flux distribution
```

## License

MIT
