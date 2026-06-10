# Synthetic sim_out fixture with known values so summary math can be checked
make_fake_sim_out <- function(Ymax = 5, nsim = 4, L_bins = 3, Amax = 2) {
  list(
    all_YPR          = matrix(seq_len(Ymax * nsim), Ymax, nsim),
    all_SPR          = matrix(seq_len(Ymax * nsim) / 100, Ymax, nsim),
    all_Prop         = matrix(seq_len(Ymax * nsim) / (Ymax * nsim), Ymax, nsim),
    all_SSB          = matrix(seq_len(Ymax * nsim) * 10, Ymax, nsim),
    all_Abundance    = matrix(seq_len(L_bins * nsim), L_bins, nsim),
    all_AgeAbundance = matrix(seq_len(Amax * nsim), Amax, nsim),
    burnin_years     = 3
  )
}

fake_vc <- function(L_bins = 3) {
  list(
    Wt_bins        = seq_len(L_bins) * 0.1,
    Vulcap_bins    = rep(0.5, L_bins),
    Vulharv_bins   = rep(0.4, L_bins),
    trophyvul_bins = rep(0.1, L_bins)
  )
}

# ---- summarize_timeseries ----------------------------------------------------

test_that("summarize_timeseries returns one row per year with expected columns", {
  s  <- make_fake_sim_out(Ymax = 5, nsim = 4)
  ts <- summarize_timeseries(s, Ymax = 5)
  expect_equal(nrow(ts), 5)
  expect_true(all(c("YPR_mean", "YPR_lower", "YPR_upper",
                    "SPR_mean", "Prop_upper", "SSB_lower", "burnin_years") %in% names(ts)))
})

test_that("summarize_timeseries means match rowMeans of the input", {
  s  <- make_fake_sim_out(Ymax = 5, nsim = 4)
  ts <- summarize_timeseries(s, Ymax = 5)
  expect_equal(ts$YPR_mean, rowMeans(s$all_YPR))
  expect_equal(ts$SSB_mean, rowMeans(s$all_SSB))
})

test_that("summarize_timeseries lower bounds are clamped at 0 and Prop_upper at 1", {
  s  <- make_fake_sim_out()
  ts <- summarize_timeseries(s, Ymax = 5)
  expect_true(all(ts$YPR_lower  >= 0))
  expect_true(all(ts$SPR_lower  >= 0))
  expect_true(all(ts$Prop_lower >= 0))
  expect_true(all(ts$Prop_upper <= 1))
})

test_that("summarize_timeseries carries burnin_years through", {
  s  <- make_fake_sim_out()
  ts <- summarize_timeseries(s, Ymax = 5)
  expect_true(all(ts$burnin_years == s$burnin_years))
})

# ---- summarize_length_data ---------------------------------------------------

test_that("summarize_length_data binds selectivity curves and abundance stats", {
  s  <- make_fake_sim_out(L_bins = 3, nsim = 4)
  vc <- fake_vc(3)
  ld <- summarize_length_data(s, bin_midpoints = c(50, 150, 250), vc = vc)
  expect_equal(nrow(ld), 3)
  expect_equal(ld$Length, c(50, 150, 250))
  expect_equal(ld$VulHarvest, vc$Vulharv_bins)
  expect_equal(ld$Abundance_mean, rowMeans(s$all_Abundance))
  expect_true(all(ld$Abundance_lower >= 0))
})

# ---- summarize_age_data ------------------------------------------------------

test_that("summarize_age_data returns one row per age with clamped lower bound", {
  s  <- make_fake_sim_out(Amax = 2, nsim = 4)
  ad <- summarize_age_data(s, Amax = 2)
  expect_equal(nrow(ad), 2)
  expect_equal(ad$Age, 1:2)
  expect_equal(ad$Abundance_mean, rowMeans(s$all_AgeAbundance))
  expect_true(all(ad$Abundance_lower >= 0))
})
