# Shared grid setup used across multiple tests
make_wc_grid <- function(Linf = 353) {
  max_length  <- ceiling(Linf * 1.2)
  length_bins <- seq(0, max_length, by = 10)
  L_bins      <- length(length_bins) - 1
  bin_midpoints <- (length_bins[-1] + length_bins[-(L_bins + 1)]) / 2
  list(length_bins = length_bins, bin_midpoints = bin_midpoints, L_bins = L_bins)
}

# ---- make_growth_matrix --------------------------------------------------------

test_that("make_growth_matrix row-sums are 1 (stochastic matrix)", {
  g <- make_wc_grid()
  gm <- make_growth_matrix(353, 0.374, 0.197, g$bin_midpoints, g$length_bins, 0.20)
  expect_true(all(abs(rowSums(gm$Growth_matrix) - 1) < 1e-9))
})

test_that("make_growth_matrix recruit_dist sums to 1", {
  g <- make_wc_grid()
  gm <- make_growth_matrix(353, 0.374, 0.197, g$bin_midpoints, g$length_bins, 0.20)
  expect_equal(sum(gm$recruit_dist), 1, tolerance = 1e-9)
})

test_that("make_growth_matrix all values non-negative", {
  g <- make_wc_grid()
  gm <- make_growth_matrix(353, 0.374, 0.197, g$bin_midpoints, g$length_bins, 0.20)
  expect_true(all(gm$Growth_matrix >= 0))
  expect_true(all(gm$recruit_dist  >= 0))
})

test_that("make_growth_matrix deterministic (CV = 0): each row has exactly one 1", {
  g <- make_wc_grid()
  gm <- make_growth_matrix(353, 0.374, 0.197, g$bin_midpoints, g$length_bins, 0)
  expect_true(all(rowSums(gm$Growth_matrix > 0) == 1))
  expect_equal(sum(gm$recruit_dist > 0), 1)
})

test_that("make_growth_matrix returns correct structure", {
  g  <- make_wc_grid()
  gm <- make_growth_matrix(353, 0.374, 0.197, g$bin_midpoints, g$length_bins, 0.20)
  expect_named(gm, c("Growth_matrix", "recruit_dist"))
  expect_equal(dim(gm$Growth_matrix), c(g$L_bins, g$L_bins))
  expect_length(gm$recruit_dist, g$L_bins)
})

# ---- make_vulnerability_curves -----------------------------------------------

test_that("make_vulnerability_curves returns values in [0, 1]", {
  g  <- make_wc_grid()
  vc <- make_vulnerability_curves(
    g$bin_midpoints, 204, 254, 180, 305, 2.40991e-6, 3.38, 0.374, 1.27
  )
  expect_true(all(vc$Vulcap_bins    >= 0 & vc$Vulcap_bins    <= 1))
  expect_true(all(vc$Vulharv_bins   >= 0 & vc$Vulharv_bins   <= 1))
  expect_true(all(vc$trophyvul_bins >= 0 & vc$trophyvul_bins <= 1))
  expect_true(all(vc$S_bins         >  0 & vc$S_bins         <= 1))
})

test_that("make_vulnerability_curves fecundity and weight are non-negative", {
  g  <- make_wc_grid()
  vc <- make_vulnerability_curves(
    g$bin_midpoints, 204, 254, 180, 305, 2.40991e-6, 3.38, 0.374, 1.27
  )
  expect_true(all(vc$Fec_bins >= 0))
  expect_true(all(vc$Wt_bins  >= 0))
})

test_that("traditional slot limits harvest to within-slot fish only", {
  g <- make_wc_grid(Linf = 600)
  vc <- make_vulnerability_curves(
    g$bin_midpoints, 200, 300, 200, 500, 8.16e-6, 3.10, 0.22, 1.18,
    enable_slot = TRUE, slot_type = "traditional", slot_upper = 450
  )
  # Fish well below min (100 mm) and well above max (550 mm) should have ~0 harvest vuln
  small_idx <- which(g$bin_midpoints < 150)
  large_idx <- which(g$bin_midpoints > 520)
  expect_true(all(vc$Vulharv_bins[small_idx] < 0.01))
  expect_true(all(vc$Vulharv_bins[large_idx] < 0.01))
})

test_that("max length limit restricts harvest above ceiling", {
  g <- make_wc_grid(Linf = 600)
  vc <- make_vulnerability_curves(
    g$bin_midpoints, 200, 250, 200, 500, 8.16e-6, 3.10, 0.22, 1.18,
    enable_max_limit = TRUE, max_harvest_size = 400
  )
  large_idx <- which(g$bin_midpoints > 450)
  expect_true(all(vc$Vulharv_bins[large_idx] < 0.01))
})

test_that("make_vulnerability_curves returns expected named list", {
  g  <- make_wc_grid()
  vc <- make_vulnerability_curves(
    g$bin_midpoints, 204, 254, 180, 305, 2.40991e-6, 3.38, 0.374, 1.27
  )
  expected_names <- c("Vulcap_bins", "Vulharv_bins", "trophyvul_bins",
                      "Fec_bins", "Wt_bins", "M_bins", "S_bins", "maturity_ogive_bins")
  expect_named(vc, expected_names, ignore.order = TRUE)
})

# ---- run_population_simulation -----------------------------------------------

# Helper: build white-crappie inputs for a fast test run
make_wc_inputs <- function(nsim = 20) {
  g  <- make_wc_grid()
  gm <- make_growth_matrix(353, 0.374, 0.197, g$bin_midpoints, g$length_bins, 0.20)
  vc <- make_vulnerability_curves(
    g$bin_midpoints, 204, 254, 180, 305, 2.40991e-6, 3.38, 0.374, 1.27
  )
  list(
    bin_midpoints = g$bin_midpoints, length_bins = g$length_bins,
    Growth_matrix = gm$Growth_matrix, recruit_dist = gm$recruit_dist,
    Vulcap_bins = vc$Vulcap_bins, Vulharv_bins = vc$Vulharv_bins,
    trophyvul_bins = vc$trophyvul_bins, Fec_bins = vc$Fec_bins,
    Wt_bins = vc$Wt_bins, S_bins = vc$S_bins,
    Amax = 8L, Ymax = 128L,
    Ro = 1000, rec_cv = 0.8, U = 0.34, DisMort = 0.09, nsim = nsim
  )
}

test_that("run_population_simulation returns expected structure (full output)", {
  set.seed(1)
  inp <- make_wc_inputs()
  out <- do.call(run_population_simulation,
                 c(inp, collect_full_output = TRUE))

  expect_named(out, c("sim_df", "burnin_years",
                      "all_YPR", "all_SPR", "all_Prop", "all_EggProd",
                      "all_Abundance", "all_AgeAbundance"),
               ignore.order = TRUE)
  expect_equal(nrow(out$sim_df), inp$nsim)
  expect_equal(ncol(out$all_YPR), inp$nsim)
  expect_equal(nrow(out$all_YPR), inp$Ymax)
})

test_that("run_population_simulation SPR is non-negative and finite", {
  set.seed(2)
  out <- do.call(run_population_simulation, c(make_wc_inputs(), collect_full_output = FALSE))
  spr <- out$sim_df$SPR
  # SPR = stock egg production relative to unfished equilibrium.
  # Stochastic (lognormal recruitment); may exceed 1 in favourable recruitment
  # years. Guaranteed invariants are finiteness and non-negativity.
  expect_true(all(is.finite(spr)))
  expect_true(all(spr >= 0))
})

test_that("run_population_simulation YPR is non-negative", {
  set.seed(3)
  out <- do.call(run_population_simulation, c(make_wc_inputs(), collect_full_output = FALSE))
  expect_true(all(out$sim_df$YPR >= 0, na.rm = TRUE))
})

test_that("run_population_simulation with U = 0 gives higher SPR than U = 0.5", {
  set.seed(42)
  inp0   <- make_wc_inputs(nsim = 50); inp0$U   <- 0.0
  inp50  <- make_wc_inputs(nsim = 50); inp50$U  <- 0.5
  out0  <- do.call(run_population_simulation, c(inp0,  collect_full_output = FALSE))
  out50 <- do.call(run_population_simulation, c(inp50, collect_full_output = FALSE))
  expect_gt(mean(out0$sim_df$SPR), mean(out50$sim_df$SPR))
})

test_that("collect_full_output = FALSE omits matrix outputs", {
  set.seed(5)
  out <- do.call(run_population_simulation, c(make_wc_inputs(), collect_full_output = FALSE))
  expect_false("all_YPR" %in% names(out))
  expect_false("all_Abundance" %in% names(out))
})

test_that("burnin_years equals min(Ymax, Amax + 20)", {
  set.seed(6)
  inp <- make_wc_inputs()
  out <- do.call(run_population_simulation, c(inp, collect_full_output = FALSE))
  expect_equal(out$burnin_years, min(inp$Ymax, inp$Amax + 20))
})

test_that("fast path (collect_full_output=FALSE) gives same sim_df as full path at same seed", {
  inp <- make_wc_inputs(nsim = 200)
  set.seed(42)
  out_full <- do.call(run_population_simulation, c(inp, list(collect_full_output = TRUE)))
  set.seed(42)
  out_fast <- do.call(run_population_simulation, c(inp, list(collect_full_output = FALSE)))
  expect_equal(out_full$sim_df, out_fast$sim_df, tolerance = 1e-8)
})

test_that("fast path matches full path with DDR enabled", {
  inp <- make_wc_inputs(nsim = 100)
  set.seed(99)
  out_full <- do.call(run_population_simulation,
                      c(inp, list(collect_full_output = TRUE,
                                  enable_ddr = TRUE, steepness = 0.7)))
  set.seed(99)
  out_fast <- do.call(run_population_simulation,
                      c(inp, list(collect_full_output = FALSE,
                                  enable_ddr = TRUE, steepness = 0.7)))
  expect_equal(out_full$sim_df, out_fast$sim_df, tolerance = 1e-8)
})

test_that("fast path matches full path at U = 0 (MeanLengthHarvested is NA, not NaN)", {
  inp   <- make_wc_inputs(nsim = 50)
  inp$U <- 0.0
  set.seed(77)
  out_full <- do.call(run_population_simulation, c(inp, list(collect_full_output = TRUE)))
  set.seed(77)
  out_fast <- do.call(run_population_simulation, c(inp, list(collect_full_output = FALSE)))
  expect_equal(out_full$sim_df, out_fast$sim_df, tolerance = 1e-8)
  expect_true(all(is.na(out_full$sim_df$MeanLengthHarvested)))
  expect_false(any(is.nan(out_full$sim_df$MeanLengthHarvested)))
})

test_that("fast path sim_df columns are finite and biologically plausible", {
  set.seed(7)
  inp <- make_wc_inputs(nsim = 50)
  out <- do.call(run_population_simulation, c(inp, list(collect_full_output = FALSE)))
  df  <- out$sim_df
  expect_true(all(is.finite(df$SPR)))
  expect_true(all(df$SPR    >= 0))
  expect_true(all(df$YPR    >= 0, na.rm = TRUE))
  expect_true(all(df$Prop   >= 0 & df$Prop   <= 1, na.rm = TRUE))
  expect_true(all(df$Recruit > 0, na.rm = TRUE))
})
