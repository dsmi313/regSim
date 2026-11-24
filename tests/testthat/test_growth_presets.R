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

test_that("non-bass, non-crappie presets use M = 1.5 * K convention", {
  # Black bass (lmb_north, lmb_south, smb) are excluded: they use a fixed species-median M from
  # Beamesderfer and North (1995), not the Jensen M = 1.5 K default.
  for (sp in c("walleye", "channel_catfish", "blue_catfish")) {
    for (pr in c("slow", "moderate", "fast")) {
      gp <- get_growth_preset(sp, pr)
      expect_equal(gp$nat_mort, 1.5 * gp$vbk, info = paste(sp, pr))
    }
  }
})

test_that("black bass presets use fixed species-median M (Beamesderfer & North 1995)", {
  # M is constant across growth scenarios and independent of K.
  fixed_m <- c(lmb_north = 0.46, lmb_south = 0.46, smb = 0.43)
  for (sp in names(fixed_m)) {
    for (pr in c("slow", "moderate", "fast")) {
      gp <- get_growth_preset(sp, pr)
      expect_equal(gp$nat_mort, unname(fixed_m[[sp]]), info = paste(sp, pr))
    }
  }
})

test_that("all presets return the four expected fields", {
  species <- c("white_crappie", "black_crappie", "walleye",
               "lmb_north", "lmb_south", "smb", "channel_catfish", "blue_catfish")
  for (sp in species) {
    for (pr in c("slow", "moderate", "fast")) {
      gp <- get_growth_preset(sp, pr)
      expect_named(gp, c("linf", "vbk", "t0", "nat_mort"), info = paste(sp, pr))
    }
  }
})

test_that("growth rate K increases from slow to fast within a species", {
  species <- c("white_crappie", "walleye", "lmb_north", "lmb_south", "smb",
               "channel_catfish", "blue_catfish")
  for (sp in species) {
    k_slow <- get_growth_preset(sp, "slow")$vbk
    k_fast <- get_growth_preset(sp, "fast")$vbk
    expect_gt(k_fast, k_slow)
  }
})

test_that("Largemouth Bass North and South presets match the growth pools", {
  # Values from inst/extdata/lmb_growth_presets.xlsx (Presets sheet)
  north <- list(slow = c(687, 0.143, -0.15), moderate = c(591, 0.185, 0.05),
                fast = c(556, 0.220, 0.18))
  south <- list(slow = c(666, 0.160, -0.75), moderate = c(619, 0.254, -0.33),
                fast = c(563, 0.280, -0.07))
  for (pr in names(north)) {
    gn <- get_growth_preset("lmb_north", pr)
    gs <- get_growth_preset("lmb_south", pr)
    expect_equal(c(gn$linf, gn$vbk, gn$t0), north[[pr]], info = paste("north", pr))
    expect_equal(c(gs$linf, gs$vbk, gs$t0), south[[pr]], info = paste("south", pr))
  }
  # Inverse pairing: Linf falls as K rises
  for (sp in c("lmb_north", "lmb_south")) {
    expect_gt(get_growth_preset(sp, "slow")$linf, get_growth_preset(sp, "fast")$linf)
  }
  # Southern fish grow faster than northern fish at every level
  for (pr in names(north)) {
    expect_gt(get_growth_preset("lmb_south", pr)$vbk, get_growth_preset("lmb_north", pr)$vbk)
  }
})

test_that("species-level Largemouth Bass growth equals the moderate preset", {
  for (sp in c("lmb_north", "lmb_south")) {
    p  <- get_species_preset(sp)
    gp <- get_growth_preset(sp, "moderate")
    expect_equal(c(p$linf, p$vbk, p$t0, p$nat_mort),
                 c(gp$linf, gp$vbk, gp$t0, gp$nat_mort), info = sp)
  }
  expect_null(get_species_preset("lmb"))
})
