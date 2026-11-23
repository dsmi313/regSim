test_that("make_length_bins returns consistent structure", {
  b <- make_length_bins(353)
  expect_named(b, c("length_bins", "bin_midpoints", "L_bins"))
  expect_length(b$length_bins, b$L_bins + 1)
  expect_length(b$bin_midpoints, b$L_bins)
})

test_that("bins span 0 to ceiling(1.6 * Linf) in 10mm steps by default", {
  b <- make_length_bins(353)
  expect_equal(b$length_bins[1], 0)
  # 353 * 1.6 = 564.8 -> ceiling 565, which is not a multiple of 10, so the
  # top edge is the last full 10mm step at or below it (560). The grid never
  # exceeds the ceiling and comes within one bin width of it.
  expect_lte(max(b$length_bins), ceiling(353 * 1.6))
  expect_gt(max(b$length_bins), ceiling(353 * 1.6) - 10)
  expect_true(all(abs(diff(b$length_bins) - 10) < 1e-9))
})

test_that("bin_midpoints are halfway between consecutive edges", {
  b <- make_length_bins(353)
  expected_mid <- (b$length_bins[-1] + b$length_bins[-length(b$length_bins)]) / 2
  expect_equal(b$bin_midpoints, expected_mid)
})

test_that("custom bin_width is honoured", {
  b <- make_length_bins(500, bin_width = 25)
  expect_true(all(abs(diff(b$length_bins) - 25) < 1e-9))
})

test_that("max_mult = 1.2 reproduces the original inline grid construction", {
  Linf <- 584
  bin_width <- 10
  max_length <- ceiling(Linf * 1.2)
  length_bins <- seq(0, max_length, by = bin_width)
  L_bins <- length(length_bins) - 1
  bin_midpoints <- (length_bins[-1] + length_bins[-(L_bins + 1)]) / 2

  # The pre-0.2.0 grid (0 to 1.2 Linf) is still available with max_mult = 1.2
  b <- make_length_bins(Linf, max_mult = 1.2)
  expect_equal(b$length_bins,   length_bins)
  expect_equal(b$bin_midpoints, bin_midpoints)
  expect_equal(b$L_bins,        L_bins)
})
