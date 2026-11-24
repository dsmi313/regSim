test_that("get_species_preset returns NULL for custom / unknown species", {
  expect_null(get_species_preset("custom"))
  expect_null(get_species_preset("nonexistent_species"))
})

test_that("white crappie preset has correct biological parameters", {
  p <- get_species_preset("white_crappie")
  expect_equal(p$linf,     353)
  expect_equal(p$vbk,      0.374)
  expect_equal(p$t0,       0.197)
  expect_equal(p$nat_mort, 0.374)   # M = K
  expect_equal(p$amax,     8)
  expect_equal(p$rec_cv,   0.8)
  expect_equal(p$fec_exp,  1.27)
})

test_that("black bass use fixed species-median M (Beamesderfer & North 1995)", {
  # Not the Jensen M = 1.5 * K default; M is independent of K for these species.
  expect_equal(get_species_preset("lmb_north")$nat_mort, 0.46)
  expect_equal(get_species_preset("lmb_south")$nat_mort, 0.46)
  expect_equal(get_species_preset("smb")$nat_mort, 0.43)
})

test_that("crappie species use fec_exp = 1.27, all others use 1.18", {
  expect_equal(get_species_preset("white_crappie")$fec_exp, 1.27)
  expect_equal(get_species_preset("black_crappie")$fec_exp, 1.27)
  for (sp in c("walleye", "lmb_north", "lmb_south", "smb", "channel_catfish", "blue_catfish")) {
    expect_equal(get_species_preset(sp)$fec_exp, 1.18, info = sp)
  }
})

test_that("ymax equals amax + 120 for all species", {
  species <- c("white_crappie", "black_crappie", "walleye",
               "lmb_north", "lmb_south", "smb", "channel_catfish", "blue_catfish")
  for (sp in species) {
    p <- get_species_preset(sp)
    expect_equal(p$ymax, p$amax + 120, info = sp)
  }
})

test_that("all species presets contain required fields", {
  required <- c("wl_a", "wl_b", "mat_size", "memorable_size", "stock_size",
                "linf", "vbk", "t0", "nat_mort",
                "rec_cv", "amax", "ymax", "capsize",
                "fec_exp", "label")
  species <- c("white_crappie", "black_crappie", "walleye",
               "lmb_north", "lmb_south", "smb", "channel_catfish", "blue_catfish")
  for (sp in species) {
    p <- get_species_preset(sp)
    for (f in required) {
      expect_true(f %in% names(p), info = paste(sp, "missing:", f))
    }
  }
})

test_that("all numeric preset values are positive", {
  species <- c("white_crappie", "black_crappie", "walleye",
               "lmb_north", "lmb_south", "smb", "channel_catfish", "blue_catfish")
  skip_fields <- c("t0", "label")   # t0 can be negative
  for (sp in species) {
    p <- get_species_preset(sp)
    for (f in setdiff(names(p), skip_fields)) {
      if (is.numeric(p[[f]])) {
        expect_true(p[[f]] > 0, info = paste(sp, f, "should be positive"))
      }
    }
  }
})

test_that("stock and memorable sizes are Gabelhouse (1984a) metric lengths", {
  stock <- c(white_crappie = 130, black_crappie = 130, walleye = 250, lmb_north = 200, lmb_south = 200,
             smb = 180, channel_catfish = 280, blue_catfish = 300)
  memorable <- c(white_crappie = 300, black_crappie = 300, walleye = 630, lmb_north = 510, lmb_south = 510,
                 smb = 430, channel_catfish = 710, blue_catfish = 890)
  for (sp in names(stock)) {
    p <- get_species_preset(sp)
    expect_equal(p$stock_size, stock[[sp]])
    expect_equal(p$memorable_size, memorable[[sp]])
    expect_lt(p$stock_size, p$memorable_size)
  }
})
