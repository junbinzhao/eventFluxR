#' Fit a class-swap model (hot-moment classifier + separate regressors)
#'
#' Implements the "class-swap" workflow used for N2O flux modeling: an
#' XGBoost classifier first labels each observation as background or hot
#' moment, then independent XGBoost regressors are fit within each class
#' (optionally up-weighting large flux values), so predictions can be
#' recombined by routing each observation through its classified branch
#' (see [predict.classswap_model()]).
#'
#' @param data A data frame containing the response, predictors and
#'   grouping columns.
#' @param response_col Name of the numeric flux response column.
#' @param feature_cols Character vector of predictor column names (used by
#'   both the classifier and the two regressors).
#' @param group_vars Character vector of grouping columns (e.g.
#'   `c("Plot", "Fertiliz")`) defining blocked cross-validation folds.
#' @param label_col Optional name of an existing binary hot-moment label
#'   used to train the classifier; if `NULL`, a MAD flag (`mad_k`) on
#'   `response_col` is used instead.
#' @param mad_k MAD threshold multiplier used to derive the label when
#'   `label_col` is `NULL` (default 3).
#' @param threshold Classifier probability threshold for calling a hot
#'   moment (default 0.65).
#' @param weight_exponent,weight_q Control up-weighting of large flux values
#'   when training the hot-moment regressor:
#'   `weight = max(1, (max(y, 0) / q)^exponent)`, where `q` is the
#'   `weight_q` quantile of `response_col` (default exponent 0 = uniform
#'   weights).
#' @param n_trials,nrounds,early_stopping_rounds,seed,verbose Passed to the
#'   XGBoost random-search tuner used for the classifier and both
#'   regressors.
#'
#' @return An object of class `"classswap_model"` with elements
#'   `classifier`, `model_bg`, `model_hot`, `feature_cols`, `threshold`, and
#'   `cv_scores` (best cross-validation score for each of the three fitted
#'   models). Use `predict()` (see [predict.classswap_model()]) to generate
#'   recombined predictions on new data.
#' @export
#'
#' @examples
#' \donttest{
#' data(n2o_sample)
#' feats <- c("CO2_flux", "Soil_Moist", "Soil_T_2", "VPD", "fert_ef")
#'
#' fit <- classswap_model(
#'   n2o_sample, response_col = "N2O_flux", feature_cols = feats,
#'   group_vars = "Plot", label_col = "y_bin",
#'   n_trials = 15, nrounds = 300, early_stopping_rounds = 30, verbose = FALSE
#' )
#' pred <- predict(fit, n2o_sample)
#' head(pred)
#' }
classswap_model <- function(data, response_col, feature_cols, group_vars,
                             label_col = NULL, mad_k = 3, threshold = 0.65,
                             weight_exponent = 0, weight_q = 0.95,
                             n_trials = 100, nrounds = 3000,
                             early_stopping_rounds = 100,
                             seed = 23, verbose = TRUE) {
  stopifnot(is.data.frame(data), response_col %in% names(data))
  stopifnot(all(feature_cols %in% names(data)))
  stopifnot(all(group_vars %in% names(data)))

  y <- data[[response_col]]
  x <- as.matrix(data[feature_cols])
  folds <- .make_group_folds(data, group_vars)

  label <- if (!is.null(label_col)) {
    stopifnot(label_col %in% names(data))
    as.integer(as.logical(data[[label_col]]))
  } else {
    as.integer(.mad_flag(y, k = mad_k))
  }

  # 1) hot-moment classifier
  dtrain_clf <- xgboost::xgb.DMatrix(data = x, label = label)
  scale_pos_weight <- sum(label == 0) / max(sum(label == 1), 1)
  clf_fit <- .train_xgb_tuned(
    dtrain_clf, folds,
    objective = "binary:logistic", eval_metric = "aucpr", maximize = TRUE,
    n_trials = n_trials, nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    extra_params = list(scale_pos_weight = scale_pos_weight),
    seed = seed, verbose = verbose
  )

  # partition training data by the classifier's own predictions, so the
  # regressors see the same class boundary used later at prediction time
  prob <- stats::predict(clf_fit$model, dtrain_clf)
  is_hot <- prob >= threshold
  ind_bg <- which(!is_hot)
  ind_hot <- which(is_hot)

  if (length(ind_bg) < 2 || length(ind_hot) < 2) {
    stop(
      "The classifier assigned fewer than 2 observations to the ",
      if (length(ind_bg) < 2) "background" else "hot-moment",
      " class at threshold = ", threshold, ", so a regressor cannot be fit ",
      "for that class. Try lowering `threshold`, increasing `n_trials`/",
      "`nrounds` so the classifier learns a less degenerate split, or check ",
      "that `label_col`/`mad_k` produce a reasonably balanced label.",
      call. = FALSE
    )
  }

  # 2) background regressor
  dtrain_bg <- xgboost::xgb.DMatrix(data = x[ind_bg, , drop = FALSE], label = y[ind_bg])
  folds_bg <- .make_group_folds(data[ind_bg, , drop = FALSE], group_vars)
  .check_cv_folds(folds_bg, "background")
  bg_fit <- .train_xgb_tuned(
    dtrain_bg, folds_bg,
    objective = "reg:squarederror", eval_metric = "rmse", maximize = FALSE,
    n_trials = n_trials, nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    seed = seed, verbose = verbose
  )

  # 3) hot-moment regressor (optionally weighted toward large flux values)
  w_hot <- .magnitude_weights(y[ind_hot], exponent = weight_exponent, q_prob = weight_q)
  dtrain_hot <- xgboost::xgb.DMatrix(data = x[ind_hot, , drop = FALSE],
                                     label = y[ind_hot], weight = w_hot)
  folds_hot <- .make_group_folds(data[ind_hot, , drop = FALSE], group_vars)
  .check_cv_folds(folds_hot, "hot-moment")
  hot_fit <- .train_xgb_tuned(
    dtrain_hot, folds_hot,
    objective = "reg:squarederror", eval_metric = "rmse", maximize = FALSE,
    n_trials = n_trials, nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    seed = seed, verbose = verbose
  )

  structure(
    list(
      classifier = clf_fit$model,
      model_bg = bg_fit$model,
      model_hot = hot_fit$model,
      feature_cols = feature_cols,
      threshold = threshold,
      cv_scores = list(classifier = clf_fit$best_score,
                       background = bg_fit$best_score,
                       hot_moment = hot_fit$best_score)
    ),
    class = "classswap_model"
  )
}

#' Predict from a fitted class-swap model
#'
#' Classifies each row of `newdata` as background or hot moment, then routes
#' it to the matching regressor and returns the recombined predictions in
#' the original row order.
#'
#' @param object A `"classswap_model"` object from [classswap_model()].
#' @param newdata A data frame or matrix containing at least
#'   `object$feature_cols`.
#' @param ... Unused, included for S3 consistency.
#'
#' @return A numeric vector of recombined predictions, one per row of
#'   `newdata`.
#' @export
predict.classswap_model <- function(object, newdata, ...) {
  newdata_mat <- as.matrix(newdata[object$feature_cols])
  dnew <- xgboost::xgb.DMatrix(data = newdata_mat)

  prob <- stats::predict(object$classifier, dnew)
  is_hot <- prob >= object$threshold

  pred <- numeric(nrow(newdata_mat))
  if (any(!is_hot)) {
    pred[!is_hot] <- stats::predict(object$model_bg,
                                    xgboost::xgb.DMatrix(newdata_mat[!is_hot, , drop = FALSE]))
  }
  if (any(is_hot)) {
    pred[is_hot] <- stats::predict(object$model_hot,
                                   xgboost::xgb.DMatrix(newdata_mat[is_hot, , drop = FALSE]))
  }
  pred
}
