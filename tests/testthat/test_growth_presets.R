test_that("get_growth_preset returns NULL for unknown species or preset", {
  expect_null(get_growth_preset("custom", "moderate"))
  expect_null(get_growth_preset("nonexistent", "slow"))
  expect_null(get_growth_preset("white_crappie", "turbo"))
})

test_that("white crappie moderate matches the species default", {
  gp <- get_growth_preset("white_crappie", "moderate")
  expect_equal(gp$linf,     353)
  expect_equal(gp$vbk,      0.374)
  expect_equal(gp$t0,       0.197)
  expect_equal(gp$nat_mort, 0.374)
})

test_that("crappie presets use M = K convention", {
  for (sp in c("white_crappie", "black_crappie")) {
    for (pr in c("slow", "moderate", "fast")) {
      gp <- get_growth_preset(sp, pr)
      expect_equal(gp$nat_mort, gp$vbk, info = paste(sp, pr))
    }
  }
})

test_that("non-crappie presets use M = 1.5 * K convention", {
  for (sp in c("walleye", "lmb", "smb", "channel_catfish", "blue_catfish")) {
    for (pr in c("slow", "moderate", "fast")) {
      gp <- get_growth_preset(sp, pr)
      expect_equal(gp$nat_mort, 1.5 * gp$vbk, info = paste(sp, pr))
    }
  }
})

test_that("all presets return the four expected fields", {
  species <- c("white_crappie", "black_crappie", "walleye",
               "lmb", "smb", "channel_catfish", "blue_catfish")
  for (sp in species) {
    for (pr in c("slow", "moderate", "fast")) {
      gp <- get_growth_preset(sp, pr)
      expect_named(gp, c("linf", "vbk", "t0", "nat_mort"), info = paste(sp, pr))
    }
  }
})

test_that("growth rate K increases from slow to fast within a species", {
  species <- c("white_crappie", "walleye", "lmb", "smb",
               "channel_catfish", "blue_catfish")
  for (sp in species) {
    k_slow <- get_growth_preset(sp, "slow")$vbk
    k_fast <- get_growth_preset(sp, "fast")$vbk
    expect_gt(k_fast, k_slow)
  }
})
