# scripts/lmb_sim.R
#
# Largemouth bass regulation scenario simulations
#
# Growth parameters: FishBase median for Micropterus salmoides; see also:
#   Allen, M.S., Pine, W.E., Walters, C.J. 2008. Temporal trends in largemouth
#   bass fisheries in Florida lakes: Estimates and simulations of contributed
#   and harvested fish. Transactions of the American Fisheries Society
#   137:1196-1207.
#
#   Allen, M.S., Walters, C.J., Myers, R. 2008. Temporal trends in largemouth
#   bass exploitation in the United States and Canada. North American Journal
#   of Fisheries Management 28:418-427.
#
# Exploitation rates: Allen, Walters, and Myers (2008, NAJFM 28:418-427)
# synthesized 32 estimates spanning 51 years; mean exploitation was 0.35
# (1976-1989) declining to 0.18 (1990-2003) as voluntary release increased.
# Range here spans typical to heavily exploited systems including tournaments.
#
# DisMort low (0.05): typical recreational C&R with artificial lures.
# Muoneke and Childress (1994, Reviews in Fisheries Science 2:123-156)
# compiled hooking mortality for 32 taxa; bass on artificial lures generally <5%.
#
# DisMort high (0.20): tournament conditions (summer heat, live-well stress,
# delayed weigh-in). Neal and Lopez-Clayton (2001, NAJFM 21:834-842) reported
# total tournament mortality averaging 42% in tropical conditions; temperate
# estimates lower but still substantially elevated relative to recreational C&R.
# Value of 0.20 represents a conservative upper bound for tournament mortality
# under typical temperate conditions.
#
# To combine with other species:
#   all_sims <- dplyr::bind_rows(crappie_simulations_df,
#                                walleye_simulations_df,
#                                lmb_simulations_df)

library(regSim)
library(progress)
library(dplyr)

# ── Progress bar ──────────────────────────────────────────────────────────────
pbar_init <- function(x) {
  progress_bar$new(
    format     = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || You got this long: :eta]",
    total      = x,
    complete   = "\U0398",
    incomplete = "\U03A6",
    current    = "\U0394",
    clear      = FALSE,
    width      = 100
  )
}

# ── Species preset ────────────────────────────────────────────────────────────
# wl_a = 8.16e-6, wl_b = 3.10  (W in kg, L in mm)
# mat_size = 203 mm (~8 in.), memorable_size = 508 mm (~20 in.)
# capsize = 280 mm (50% capture vulnerability)
# rec_cv = 0.5, amax = 12, ymax = 132, fec_exp = 1.18
sp <- get_species_preset("lmb")

# ── Growth presets ────────────────────────────────────────────────────────────
# Moderate growth approximates FishBase median for largemouth bass.
# Slow/fast represent the range across different lake productivity levels.
#   Slow:     Linf=638 mm, K=0.17 yr-1, t0=-0.21
#   Moderate: Linf=584 mm, K=0.22 yr-1, t0= 0.00  (FishBase median)
#   Fast:     Linf=540 mm, K=0.28 yr-1, t0= 0.10
growth_slow     <- get_growth_preset("lmb", "slow")
growth_moderate <- get_growth_preset("lmb", "moderate")
growth_fast     <- get_growth_preset("lmb", "fast")

# ── Exploitation rates — Allen et al. 2008 ───────────────────────────────────
# Prior and elevated estimates at low / moderate / high effort.
# Mean exploitation: 0.35 (1976-1989), 0.18 (1990-2003) as voluntary release
# increased (Allen et al. 2008, NAJFM 28:418-427). Range spans typical to
# heavily fished tournament systems.
U_df <- data.frame(
  U_label    = c("Low-Prior",  "Low-Elevated",
                 "Mod-Prior",  "Mod-Elevated",
                 "High-Prior", "High-Elevated"),
  U          = c(0.10, 0.15, 0.25, 0.35, 0.50, 0.60),
  U_category = c("Low", "Low", "Moderate", "Moderate", "High", "High"),
  stringsAsFactors = FALSE
)

# ── Simulation settings ───────────────────────────────────────────────────────
Ro   <- 10000L
nsim <- 10000L

# ── Regulation scenario parameters ───────────────────────────────────────────
# 2×2 design: regulation (min. length vs. protective slot) × discard mortality
# (low = recreational C&R, high = tournament conditions).

# Scenario 1: Minimum length 305 mm, low discard mortality
scen1_name        <- "Min. length 305 mm, low mortality"
scen1_Harvlim     <- 305
scen1_enable_slot <- FALSE
scen1_slot_type   <- "traditional"
scen1_slot_upper  <- NA_real_
scen1_DisMort     <- 0.05   # Muoneke and Childress 1994 (<5% for artificial lures)

# Scenario 2: Minimum length 305 mm, high discard mortality
scen2_name        <- "Min. length 305 mm, high mortality"
scen2_Harvlim     <- 305
scen2_enable_slot <- FALSE
scen2_slot_type   <- "traditional"
scen2_slot_upper  <- NA_real_
scen2_DisMort     <- 0.20   # tournament conditions; Neal and Lopez-Clayton 2001

# Scenario 3: Protective slot 305–508 mm, low discard mortality
scen3_name        <- "Protective slot 305-508 mm, low mortality"
scen3_Harvlim     <- 305
scen3_enable_slot <- TRUE
scen3_slot_type   <- "protective"
scen3_slot_upper  <- 508
scen3_DisMort     <- 0.05   # Muoneke and Childress 1994 (<5% for artificial lures)

# Scenario 4: Protective slot 305–508 mm, high discard mortality
scen4_name        <- "Protective slot 305-508 mm, high mortality"
scen4_Harvlim     <- 305
scen4_enable_slot <- TRUE
scen4_slot_type   <- "protective"
scen4_slot_upper  <- 508
scen4_DisMort     <- 0.20   # tournament conditions; Neal and Lopez-Clayton 2001

# ── Scenario table ────────────────────────────────────────────────────────────
scen_params <- data.frame(
  scenario    = c(scen1_name,        scen2_name,        scen3_name,        scen4_name),
  Harvlim     = c(scen1_Harvlim,     scen2_Harvlim,     scen3_Harvlim,     scen4_Harvlim),
  enable_slot = c(scen1_enable_slot, scen2_enable_slot, scen3_enable_slot, scen4_enable_slot),
  slot_type   = c(scen1_slot_type,   scen2_slot_type,   scen3_slot_type,   scen4_slot_type),
  slot_upper  = c(scen1_slot_upper,  scen2_slot_upper,  scen3_slot_upper,  scen4_slot_upper),
  DisMort     = c(scen1_DisMort,     scen2_DisMort,     scen3_DisMort,     scen4_DisMort),
  stringsAsFactors = FALSE
)

growth_labels <- c("slow", "moderate", "fast")

# Full crossing: 3 scenarios × 3 growth × 6 U = 54 combinations
combos <- merge(
  merge(scen_params,
        data.frame(growth_preset = growth_labels, stringsAsFactors = FALSE),
        by = character()),
  U_df,
  by = character()
)
combos <- combos[order(combos$scenario, combos$growth_preset, combos$U), ]
rownames(combos) <- NULL

n_combos <- nrow(combos)  # 72

cat("Largemouth bass simulation\n")
cat("  Combinations :", n_combos, "(4 scenarios x 3 growth x 6 U)\n")
cat("  Replicates   :", nsim, "per combination\n")
cat("  Total ticks  :", n_combos * nsim, "\n\n")

# ── Main simulation loop ──────────────────────────────────────────────────────
pbar <- pbar_init(n_combos * nsim)

results_list <- vector("list", n_combos)

for (i in seq_len(n_combos)) {

  combo <- combos[i, ]

  growth <- switch(combo$growth_preset,
    slow     = growth_slow,
    moderate = growth_moderate,
    fast     = growth_fast
  )

  # ── Step 1: Length bins ───────────────────────────────────────────────────
  bins <- make_length_bins(Linf = growth$linf)

  # ── Step 2: Growth matrix + recruit distribution ──────────────────────────
  gmat <- make_growth_matrix(
    Linf          = growth$linf,
    vbk           = growth$vbk,
    t0            = growth$t0,
    bin_midpoints = bins$bin_midpoints,
    length_bins   = bins$length_bins,
    growth_cv     = 0.20
  )

  # ── Step 3: Vulnerability and life-history curves ─────────────────────────
  vc <- make_vulnerability_curves(
    bin_midpoints  = bins$bin_midpoints,
    Capsize        = sp$capsize,
    Harvlim        = combo$Harvlim,
    mat_size       = sp$mat_size,
    memorable_size = sp$memorable_size,
    wl_a           = sp$wl_a,
    wl_b           = sp$wl_b,
    nat_mort       = growth$nat_mort,
    fec_exp        = sp$fec_exp,
    enable_slot    = combo$enable_slot,
    slot_type      = combo$slot_type,
    slot_upper     = if (!is.na(combo$slot_upper)) combo$slot_upper else NULL
  )

  # ── Step 4: Population simulation ─────────────────────────────────────────
  sim_out <- run_population_simulation(
    bin_midpoints       = bins$bin_midpoints,
    length_bins         = bins$length_bins,
    Growth_matrix       = gmat$Growth_matrix,
    recruit_dist        = gmat$recruit_dist,
    Vulcap_bins         = vc$Vulcap_bins,
    Vulharv_bins        = vc$Vulharv_bins,
    trophyvul_bins      = vc$trophyvul_bins,
    Fec_bins            = vc$Fec_bins,
    Wt_bins             = vc$Wt_bins,
    S_bins              = vc$S_bins,
    Amax                = sp$amax,
    Ymax                = sp$ymax,
    Ro                  = Ro,
    rec_cv              = sp$rec_cv,
    U                   = combo$U,
    DisMort             = combo$DisMort,
    nsim                = nsim,
    collect_full_output = FALSE,
    progress_fn         = function(k, n) suppressWarnings(pbar$tick())
  )

  # ── Step 5: Tag with metadata ─────────────────────────────────────────────
  d               <- sim_out$sim_df
  d$species       <- "lmb"
  d$scenario      <- combo$scenario
  d$growth_preset <- combo$growth_preset
  d$U_label       <- combo$U_label
  d$U             <- combo$U
  d$U_category    <- combo$U_category
  d$Harvlim       <- combo$Harvlim
  d$DisMort       <- combo$DisMort

  results_list[[i]] <- d
}

# ── Assemble and save ─────────────────────────────────────────────────────────
lmb_simulations_df <- bind_rows(results_list)

cat("\nDone.\n")
cat("  lmb_simulations_df:", nrow(lmb_simulations_df), "rows,",
    ncol(lmb_simulations_df), "cols\n")
cat("  Columns:", paste(names(lmb_simulations_df), collapse = ", "), "\n")

saveRDS(lmb_simulations_df, "scripts/lmb_sim_results.rds")
cat("  Saved -> scripts/lmb_sim_results.rds\n")
