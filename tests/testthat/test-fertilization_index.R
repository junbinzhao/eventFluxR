test_that("fertilization_index sums decayed contributions from past events only", {
  ts <- as.Date("2024-05-10")
  ev <- as.Date(c("2024-05-02", "2024-05-30")) # second event is in the future
  out <- fertilization_index(ts, ev, event_doses = c(90, 36), k = 0.05)
  expect_equal(out$fert_index, 90 * exp(-0.05 * 8))
  expect_equal(out$days_since_fertiliz, 8)
})

test_that("fertilization_index half_life overrides k", {
  ts <- as.Date("2024-05-15")
  ev <- as.Date("2024-05-01")
  out_k <- fertilization_index(ts, ev, event_doses = 100, k = log(2) / 14)
  out_hl <- fertilization_index(ts, ev, event_doses = 100, half_life = 14)
  expect_equal(out_k$fert_index, out_hl$fert_index)
})

test_that("fertilization_index returns 0 / NA before any event has occurred", {
  ts <- as.Date(c("2024-01-01", "2024-01-10"))
  ev <- as.Date("2024-01-05")
  out <- fertilization_index(ts, ev, event_doses = 50)
  expect_equal(out$fert_index[1], 0)
  expect_true(is.na(out$days_since_fertiliz[1]))
  expect_equal(out$days_since_fertiliz[2], 5)
})
