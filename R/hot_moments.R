#' Identify hot moments in a flux time series (MAD or XGBoost-based)
#'
#' Detects anomalously large "hot moment" flux observations either with a
#' robust MAD-based rule, or by training an XGBoost classifier to reproduce
#' (and generalize) a binary hot-moment label from environmental predictors.
#' Optionally reports a threshold-sensitivity table, and groups consecutive
#' flagged observations within each group into discrete events with a
#' duration.
#'
#' @param data A data frame of observations.
#' @param flux_col Name of the numeric flux column. Required for
#'   `method = "mad"`, and also used as the fallback training label for
#'   `method = "xgboost"` when `label_col` is `NULL`.
#' @param method `"mad"` (default) or `"xgboost"`.
#' @param group_vars Character vector of grouping columns (e.g.
#'   `c("Plot", "Fertiliz")`). Used to compute the MAD center/scale
#'   independently per group, and/or as blocked cross-validation folds for
#'   the XGBoost classifier.
#' @param k MAD threshold multiplier (default 3).
#' @param k_values Optional numeric vector of candidate `k` thresholds; if
#'   supplied (method = "mad"), a sensitivity table of flagged-point
#'   counts/percentages across `k_values` is returned in `$sensitivity`.
#' @param center One of `"median"` (default) or `"mean"`.
#' @param scale One of `"robust_sd"` (default) or `"mad"`.
#' @param constant Scaling constant converting MAD to a robust SD estimate
#'   (default 1.4826).
#' @param one_sided If `TRUE` (default), only values above the center are
#'   flagged.
#' @param label_col Name of an existing binary (0/1 or logical) hot-moment
#'   label column to train the XGBoost classifier against (method =
#'   "xgboost"). If `NULL`, a MAD flag computed from `flux_col` (with `k`)
#'   is used as the training label.
#' @param feature_cols Character vector of predictor columns (method =
#'   "xgboost").
#' @param threshold Probability threshold used to call a hot moment from
#'   the classifier (method = "xgboost"; default 0.65).
#' @param thresholds_eval Threshold grid used to report a detection-rate /
#'   false-alarm-rate sensitivity table for the classifier (method =
#'   "xgboost").
#' @param n_trials,nrounds,early_stopping_rounds,seed,verbose Passed to the
#'   XGBoost random-search tuner (method = "xgboost").
#' @param time_col Name of a timestamp column; if supplied together with
#'   `group_vars` and `compute_events = TRUE`, consecutive flagged
#'   observations are grouped into discrete events.
#' @param compute_events Whether to compute event grouping/duration when
#'   `time_col` and `group_vars` are available (default `TRUE`).
#' @param min_duration_hours Minimum duration assigned to single-point
#'   events (default 1).
#'
#' @return A list of class `"hot_moments"` containing `method` and `data`
#'   (the input data with an added `.hot_moment` logical column). For
#'   `method = "mad"` with `k_values` supplied, also includes `sensitivity`
#'   (flagged-count table across `k_values`). For `method = "xgboost"`, also
#'   includes `model` (the fitted `xgb.Booster`), `best_params`,
#'   `threshold`, and `sensitivity` (a detection-rate/false-alarm-rate table
#'   across `thresholds_eval`). If event grouping is available, an `events`
#'   data frame is included.
#' @export
#'
#' @examples
#' data(n2o_sample)
#'
#' # MAD-based detection within each chamber, plus a k-sensitivity check and
#' # event grouping/duration
#' hm_mad <- identify_hot_moments(
#'   n2o_sample, flux_col = "N2O_flux",
#'   group_vars = "Chamber", k = 3, k_values = seq(1, 6, 0.5),
#'   time_col = "Timestamp"
#' )
#' table(hm_mad$data$.hot_moment)
#' hm_mad$sensitivity
#' head(hm_mad$events)
#'
#' \donttest{
#' # XGBoost classifier reproducing the existing y_bin hot-moment label
#' hm_xgb <- identify_hot_moments(
#'   n2o_sample, method = "xgboost",
#'   label_col = "y_bin",
#'   feature_cols = c("CO2_flux", "Soil_Moist", "Soil_T_2", "VPD"),
#'   group_vars = "Plot",
#'   n_trials = 15, nrounds = 300, early_stopping_rounds = 30, verbose = FALSE
#' )
#' hm_xgb$sensitivity
#' }
identify_hot_moments <- function(data, flux_col = NULL,
                                  method = c("mad", "xgboost"),
                                  group_vars = NULL,
                                  k = 3, k_values = NULL,
                                  center = c("median", "mean"),
                                  scale = c("robust_sd", "mad"),
                                  constant = 1.4826, one_sided = TRUE,
                                  label_col = NULL, feature_cols = NULL,
                                  threshold = 0.65,
                                  thresholds_eval = seq(0.1, 0.9, by = 0.05),
                                  n_trials = 100, nrounds = 3000,
                                  early_stopping_rounds = 100,
                                  seed = 23, verbose = TRUE,
                                  time_col = NULL, compute_events = TRUE,
                                  min_duration_hours = 1) {
  method <- match.arg(method)
  center <- match.arg(center)
  scale <- match.arg(scale)
  stopifnot(is.data.frame(data))

  out <- list(method = method)

  if (method == "mad") {
    stopifnot(!is.null(flux_col), flux_col %in% names(data))

    if (is.null(group_vars)) {
      data$.hot_moment <- .mad_flag(data[[flux_col]], k = k, center = center,
                                    scale = scale, constant = constant,
                                    one_sided = one_sided)
    } else {
      stopifnot(all(group_vars %in% names(data)))
      grp <- interaction(data[group_vars], drop = TRUE)
      data$.hot_moment <- NA
      for (idx in split(seq_len(nrow(data)), grp)) {
        data$.hot_moment[idx] <- .mad_flag(data[[flux_col]][idx], k = k,
                                           center = center, scale = scale,
                                           constant = constant,
                                           one_sided = one_sided)
      }
    }
    out$k <- k

    if (!is.null(k_values)) {
      out$sensitivity <- .mad_threshold_sensitivity(
        data[[flux_col]], k_values = k_values,
        center = center, scale = scale, constant = constant,
        one_sided = one_sided
      )
    }

  } else {
    stopifnot(!is.null(feature_cols), all(feature_cols %in% names(data)))
    stopifnot(!is.null(group_vars), all(group_vars %in% names(data)))

    y <- if (!is.null(label_col)) {
      stopifnot(label_col %in% names(data))
      as.integer(as.logical(data[[label_col]]))
    } else {
      stopifnot(!is.null(flux_col), flux_col %in% names(data))
      as.integer(.mad_flag(data[[flux_col]], k = k, center = center,
                           scale = scale, constant = constant,
                           one_sided = one_sided))
    }

    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(data[feature_cols]), label = y)
    folds <- .make_group_folds(data, group_vars)
    scale_pos_weight <- sum(y == 0) / max(sum(y == 1), 1)

    fit <- .train_xgb_tuned(
      dtrain, folds,
      objective = "binary:logistic", eval_metric = "aucpr", maximize = TRUE,
      n_trials = n_trials, nrounds = nrounds,
      early_stopping_rounds = early_stopping_rounds,
      extra_params = list(scale_pos_weight = scale_pos_weight),
      seed = seed, verbose = verbose
    )

    prob <- stats::predict(fit$model, dtrain)
    data$.hot_moment_prob <- prob
    data$.hot_moment <- prob >= threshold

    out$model <- fit$model
    out$best_params <- fit$best_params
    out$threshold <- threshold
    out$sensitivity <- .detection_metrics(y, prob, thresholds = thresholds_eval)
  }

  out$data <- data

  if (compute_events && !is.null(time_col) && !is.null(group_vars)) {
    out$events <- .event_duration(data, time_col = time_col,
                                  flag_col = ".hot_moment",
                                  group_vars = group_vars,
                                  min_duration_hours = min_duration_hours)
  }

  class(out) <- "hot_moments"
  out
}
