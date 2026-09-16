test_that("identify_hot_moments (MAD) flags obvious outliers, per group", {
  set.seed(1)
  df <- data.frame(
    Chamber = rep(c("A", "B"), each = 52),
    Timestamp = rep(seq(as.POSIXct("2024-01-01", tz = "UTC"), by = "hour", length.out = 52), 2),
    flux = c(stats::rnorm(50, 1, 0.2), 10, 11,
            stats::rnorm(50, 2, 0.2), 20, 21)
  )

  hm <- identify_hot_moments(df, flux_col = "flux", group_vars = "Chamber",
                             k = 3, time_col = "Timestamp")

  expect_s3_class(hm, "hot_moments")
  expect_equal(hm$method, "mad")
  expect_true(all(hm$data$.hot_moment[c(51, 52)]))
  expect_true(all(hm$data$.hot_moment[c(103, 104)]))
  expect_false(any(hm$data$.hot_moment[c(1:50, 53:102)]))

  # events should collapse each two-point run into one event per chamber
  expect_equal(nrow(hm$events), 2)
  expect_equal(hm$events$n_points, c(2, 2))
})

test_that("identify_hot_moments (MAD) reports a k-sensitivity table when requested", {
  x <- c(stats::rnorm(100, 1, 0.2), 8, 9)
  df <- data.frame(flux = x)
  hm <- identify_hot_moments(df, flux_col = "flux", k_values = c(1, 3, 6))
  expect_equal(nrow(hm$sensitivity), 3)
  expect_true(all(diff(hm$sensitivity$n_flagged) <= 0))
})

test_that("identify_hot_moments (xgboost) trains a classifier and returns diagnostics", {
  skip_if_not_installed("xgboost")
  set.seed(1)
  n <- 60
  df <- data.frame(
    Chamber = rep(c("A", "B"), each = n / 2),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  df$y_bin <- as.integer(df$x1 + df$x2 > 1)

  hm <- identify_hot_moments(
    df, method = "xgboost", label_col = "y_bin",
    feature_cols = c("x1", "x2"), group_vars = "Chamber",
    n_trials = 2, nrounds = 20, early_stopping_rounds = 5, verbose = FALSE
  )

  expect_s3_class(hm, "hot_moments")
  expect_equal(hm$method, "xgboost")
  expect_true(is.numeric(hm$data$.hot_moment_prob))
  expect_true(nrow(hm$sensitivity) > 0)
})
