test_that("run_model_checks passes for a healthy population", {
  df <- data.frame(SPR = rep(0.55, 20), YPR = rep(0.12, 20))
  res <- run_model_checks(df)
  expect_true(res$pass)
  expect_length(res$warnings, 0)
})

test_that("run_model_checks flags SPR < 0.30", {
  df <- data.frame(SPR = rep(0.22, 20))
  res <- run_model_checks(df)
  expect_false(res$pass)
  expect_true(any(grepl("0.30", res$warnings) | grepl("overfishing", res$warnings)))
})

test_that("run_model_checks flags SPR > 1.0", {
  df <- data.frame(SPR = rep(1.15, 20))
  res <- run_model_checks(df)
  expect_false(res$pass)
  expect_true(any(grepl("1.0|exceed", res$warnings, ignore.case = TRUE)))
})

test_that("run_model_checks flags U > 0.8", {
  df  <- data.frame(SPR = rep(0.45, 20))
  res <- run_model_checks(df, list(U = 0.9))
  expect_false(res$pass)
  expect_true(any(grepl("0.80|unrealistic", res$warnings, ignore.case = TRUE)))
})

test_that("run_model_checks flags rec_cv > 1.5", {
  df  <- data.frame(SPR = rep(0.45, 20))
  res <- run_model_checks(df, list(rec_cv = 1.8))
  expect_false(res$pass)
  expect_true(any(grepl("1.5|CV", res$warnings, ignore.case = TRUE)))
})

test_that("run_model_checks can return multiple warnings simultaneously", {
  df  <- data.frame(SPR = rep(0.2, 20))
  res <- run_model_checks(df, list(U = 0.95, rec_cv = 2.0))
  expect_gte(length(res$warnings), 2L)
})

test_that("run_model_checks handles NA SPR gracefully", {
  df  <- data.frame(SPR = rep(NA_real_, 10))
  expect_no_error(run_model_checks(df))
})
