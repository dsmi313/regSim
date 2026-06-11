test_that("get_uncertainty_cv maps all levels correctly", {
  expect_equal(get_uncertainty_cv("Off"),    0.00)
  expect_equal(get_uncertainty_cv("Low"),    0.10)
  expect_equal(get_uncertainty_cv("Medium"), 0.20)
  expect_equal(get_uncertainty_cv("High"),   0.30)
})

test_that("get_uncertainty_cv throws an error for invalid level", {
  expect_error(get_uncertainty_cv("Bad"))
  expect_error(get_uncertainty_cv("MEDIUM"))
  expect_error(get_uncertainty_cv(""))
})

test_that("sample_mortality_parameters with cv=0 returns exact point estimates", {
  df <- sample_mortality_parameters(
    nat_mort = 0.35, U = 0.34, DisMort = 0.09, cv = 0, n = 200
  )
  expect_true(all(df$nat_mort == 0.35))
  expect_true(all(df$U        == 0.34))
  expect_true(all(df$DisMort  == 0.09))
})

test_that("sampled nat_mort values are always positive", {
  set.seed(42)
  df <- sample_mortality_parameters(
    nat_mort = 0.35, U = 0.34, DisMort = 0.09, cv = 0.30, n = 500
  )
  expect_true(all(df$nat_mort > 0))
})

test_that("sampled U values are bounded in [0, 1]", {
  set.seed(42)
  df <- sample_mortality_parameters(
    nat_mort = 0.35, U = 0.34, DisMort = 0.09, cv = 0.30, n = 500
  )
  expect_true(all(df$U >= 0))
  expect_true(all(df$U <= 1))
})

test_that("sampled DisMort values are bounded in [0, 1]", {
  set.seed(42)
  df <- sample_mortality_parameters(
    nat_mort = 0.35, U = 0.34, DisMort = 0.09, cv = 0.30, n = 500
  )
  expect_true(all(df$DisMort >= 0))
  expect_true(all(df$DisMort <= 1))
})

test_that("summarize_uncertainty_results returns required columns and row count", {
  set.seed(7)
  fake <- data.frame(
    YPR                 = runif(100, 0.01, 0.10),
    SPR                 = runif(100, 0.20, 0.80),
    Prop                = runif(100, 0.00, 0.30),
    MeanLengthHarvested = runif(100, 200,  350)
  )
  summ <- summarize_uncertainty_results(fake)
  expect_true(all(c("metric", "median", "lower95", "upper95") %in% names(summ)))
  expect_equal(nrow(summ), 4)
  expect_true(all(summ$lower95 <= summ$median))
  expect_true(all(summ$median  <= summ$upper95))
})
