# Build CV fold indices from the interaction of group_vars (e.g. Plot x Fertiliz)
.make_group_folds <- function(data, group_vars) {
  grp <- interaction(data[group_vars], drop = TRUE)
  lapply(levels(grp), function(s) which(grp == s))
}

# Fail early with a clear message instead of a cryptic xgboost error when a
# class subset spans fewer than 2 groups (xgb.cv requires >= 2 folds)
.check_cv_folds <- function(folds, label) {
  if (length(folds) < 2) {
    stop(
      "The ", label, " subset only spans ", length(folds), " group(s) after ",
      "classification, so cross-validation folds cannot be built. Try a ",
      "coarser `group_vars`, a different `threshold`, or more training data.",
      call. = FALSE
    )
  }
}

# Upper-magnitude sample weights: w = max(1, (max(y,0)/q)^exponent)
.magnitude_weights <- function(y, exponent = 1, q_prob = 0.95) {
  q <- stats::quantile(y, probs = q_prob, na.rm = TRUE)
  pmax(1, (pmax(y, 0) / q)^exponent)
}

# Random-search XGBoost hyperparameter tuning + final refit; shared engine
# behind identify_hot_moments(method = "xgboost") and classswap_model()
.train_xgb_tuned <- function(dtrain, folds,
                             objective, eval_metric, maximize,
                             n_trials = 100,
                             nrounds = 3000,
                             early_stopping_rounds = 100,
                             param_sampler = NULL,
                             extra_params = list(),
                             seed = 23,
                             verbose = TRUE) {

  if (is.null(param_sampler)) {
    param_sampler <- function() {
      list(
        max_depth = sample(3:10, 1),
        min_child_weight = sample(c(1, 2, 5, 10, 15, 20, 30), 1),
        eta = 10^stats::runif(1, log10(0.01), log10(1)),
        subsample = stats::runif(1, 0.6, 1.0),
        colsample_bytree = stats::runif(1, 0.6, 1.0),
        gamma = stats::runif(1, 0, 5),
        lambda = 10^stats::runif(1, log10(0.5), log10(10)),
        alpha = 10^stats::runif(1, log10(1e-4), log10(1))
      )
    }
  }

  score_params <- function(params, trial_seed) {
    set.seed(trial_seed)
    cv <- xgboost::xgb.cv(
      params = params,
      data = dtrain,
      folds = folds,
      nrounds = nrounds,
      early_stopping_rounds = early_stopping_rounds,
      maximize = maximize,
      verbose = 0
    )
    # xgboost >= 2.x moved best_iteration under cv$early_stop
    best_iter <- if (!is.null(cv$best_iteration)) cv$best_iteration else cv$early_stop$best_iteration
    metric_col <- paste0("test_", eval_metric, "_mean")
    best_score <- cv$evaluation_log[[metric_col]][best_iter]
    list(best_iter = best_iter, best_score = best_score)
  }

  set.seed(seed)
  results <- vector("list", n_trials)

  for (i in seq_len(n_trials)) {
    p <- c(list(objective = objective, eval_metric = eval_metric),
           param_sampler(), extra_params)

    sc <- score_params(p, trial_seed = seed + i)

    results[[i]] <- list(trial = i, params = p,
                         best_iter = sc$best_iter, best_score = sc$best_score)

    if (verbose) {
      cat(sprintf("Trial %d: %s=%.4f @ iter=%d\n",
                  i, eval_metric, sc$best_score, sc$best_iter))
    }
  }

  scores <- vapply(results, `[[`, numeric(1), "best_score")
  best_idx <- if (maximize) which.max(scores) else which.min(scores)
  best <- results[[best_idx]]

  final_model <- xgboost::xgb.train(
    params = best$params,
    data = dtrain,
    nrounds = best$best_iter,
    verbose = 0
  )

  list(model = final_model,
       best_params = best$params,
       best_nrounds = best$best_iter,
       best_score = best$best_score,
       search_results = results)
}
