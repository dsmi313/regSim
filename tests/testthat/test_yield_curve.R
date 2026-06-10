# Build white-crappie engine inputs for a small yield-curve run
make_yc_inputs <- function() {
  b  <- make_length_bins(353)
  gm <- make_growth_matrix(353, 0.374, 0.197, b$bin_midpoints, b$length_bins, 0.20)
  vc <- make_vulnerability_curves(
    b$bin_midpoints, 204, 254, 180, 305, 2.40991e-6, 3.38, 0.374, 1.27
  )
  list(
    bin_midpoints = b$bin_midpoints, length_bins = b$length_bins,
    Growth_matrix = gm$Growth_matrix, recruit_dist = gm$recruit_dist,
    Vulcap_bins = vc$Vulcap_bins, Vulharv_bins = vc$Vulharv_bins,
    trophyvul_bins = vc$trophyvul_bins, Fec_bins = vc$Fec_bins,
    Wt_bins = vc$Wt_bins, S_bins = vc$S_bins,
    Amax = 8L, Ymax = 128L,
    Ro = 1000, rec_cv = 0.8, DisMort = 0.09, nsim = 10
  )
}

test_that("run_yield_curve returns one row per U with expected columns", {
  set.seed(10)
  out <- do.call(run_yield_curve,
                 c(make_yc_inputs(), list(U_values = c(0, 0.3, 0.6))))
  expect_equal(nrow(out), 3)
  expect_equal(out$U, c(0, 0.3, 0.6))
  expect_true(all(c("YPR_mean", "SPR_mean", "Prop_mean",
                    "Recruit_mean", "TotalYield_mean") %in% names(out)))
})

test_that("run_yield_curve records nsim in the *_n columns", {
  set.seed(11)
  out <- do.call(run_yield_curve,
                 c(make_yc_inputs(), list(U_values = c(0, 0.5))))
  expect_true(all(out$YPR_n == 10))
  expect_true(all(out$SPR_n == 10))
})

test_that("run_yield_curve SPR decreases as exploitation increases", {
  set.seed(12)
  out <- do.call(run_yield_curve,
                 c(make_yc_inputs(), list(U_values = c(0, 0.4, 0.8))))
  expect_true(out$SPR_mean[1] >= out$SPR_mean[2])
  expect_true(out$SPR_mean[2] >= out$SPR_mean[3])
})

test_that("run_yield_curve invokes the progress callback once per U", {
  set.seed(13)
  calls <- 0L
  do.call(run_yield_curve,
          c(make_yc_inputs(),
            list(U_values = c(0, 0.5),
                 progress_fn = function(i, n) calls <<- calls + 1L)))
  expect_equal(calls, 2L)
})

# ---- compute_msy -------------------------------------------------------------

test_that("compute_msy locates the row of maximum total yield", {
  curve <- data.frame(
    U               = c(0.0, 0.2, 0.4, 0.6),
    TotalYield_mean = c(0,   50,  90,  70)
  )
  msy <- compute_msy(curve)
  expect_equal(msy$idx, 3)
  expect_equal(msy$U, 0.4)
  expect_equal(msy$total_yield, 90)
})

test_that("compute_msy returns the expected named structure", {
  curve <- data.frame(U = c(0, 0.5), TotalYield_mean = c(10, 20))
  expect_named(compute_msy(curve), c("idx", "U", "total_yield"))
})
