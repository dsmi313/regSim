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

test_that("summarize_uncertainty_timeseries pools draws into per-year bands", {
  set.seed(11)
  Ymax  <- 30
  ndraw <- 8
  ninner <- 5
  # One Ymax x ninner matrix per parameter draw, for each metric.
  mk <- function(scale) lapply(seq_len(ndraw), function(d)
    matrix(runif(Ymax * ninner, 0, scale), nrow = Ymax, ncol = ninner))
  ypr  <- mk(0.1); spr <- mk(1); prop <- mk(0.3); egg <- mk(1e6)

  band <- summarize_uncertainty_timeseries(ypr, spr, prop, egg, Ymax)

  expect_equal(nrow(band), Ymax)
  expect_true(all(c("Year",
                    "YPR_med", "YPR_lower", "YPR_upper",
                    "SPR_med", "SPR_lower", "SPR_upper",
                    "Prop_med", "Prop_lower", "Prop_upper",
                    "EggProd_med", "EggProd_lower", "EggProd_upper") %in% names(band)))
  # Band must be ordered lower <= median <= upper in every year and metric.
  expect_true(all(band$YPR_lower  <= band$YPR_med  & band$YPR_med  <= band$YPR_upper))
  expect_true(all(band$SPR_lower  <= band$SPR_med  & band$SPR_med  <= band$SPR_upper))
  expect_true(all(band$Prop_lower <= band$Prop_med & band$Prop_med <= band$Prop_upper))
  expect_true(all(band$EggProd_lower <= band$EggProd_med &
                  band$EggProd_med   <= band$EggProd_upper))
})
