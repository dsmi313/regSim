# scripts/crappie_sim.R
#
# White crappie regulation scenario simulations
#
# Citation:
#   Smith, D.R., Bennett, D.L., Norman, J.D., Allen, M.S. 2025.
#   Live-imaging sonar use in Texas crappie fisheries: Examining
#   population-level responses due to potential increases in exploitation.
#   Fisheries, vuae015. https://doi.org/10.1093/fshmag/vuae015
#
# This script runs every regSim function call explicitly (no wrappers) so the
# model mechanics are visible at each step. The output is crappie_simulations_df
# — raw per-replicate data for you to summarize and plot yourself.
#
# To combine with other species later:
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
    complete   = "\U0398",   # Θ
    incomplete = "\U03A6",   # Φ
    current    = "\U0394",   # Δ
    clear      = FALSE,
    width      = 100
  )
}

# ── Species preset ────────────────────────────────────────────────────────────
# wl_a = 2.41e-6, wl_b = 3.38  (W in kg, L in mm)
# mat_size = 180 mm, memorable_size = 305 mm (12 in.)
# capsize = 204 mm (50% capture vulnerability)
# rec_cv = 0.8, amax = 8, ymax = 128, fec_exp = 1.27
sp <- get_species_preset("white_crappie")

# ── Growth presets ────────────────────────────────────────────────────────────
# Moderate growth matches Smith et al. 2025 population.
# Slow and fast bracket the range observed in Texas crappie stocks.
#   Slow:     Linf=333 mm, K=0.325 yr-1, t0= 0.174
#   Moderate: Linf=353 mm, K=0.374 yr-1, t0= 0.197  (Smith et al. 2025)
#   Fast:     Linf=356 mm, K=0.691 yr-1, t0=-0.056
growth_slow     <- get_growth_preset("white_crappie", "slow")
growth_moderate <- get_growth_preset("white_crappie", "moderate")
growth_fast     <- get_growth_preset("white_crappie", "fast")

# ── Exploitation rates — Smith et al. 2025 ───────────────────────────────────
# Six rates: prior creel-survey estimates paired with live-sonar-inflated
# Three representative exploitation levels for scenario comparison.
U_df <- data.frame(
  U_label    = c("Low", "Moderate", "High"),
  U          = c(0.30,  0.50,       0.70),
  U_category = c("Low", "Moderate", "High"),
  stringsAsFactors = FALSE
)

# ── Simulation settings ───────────────────────────────────────────────────────
Ro   <- 10000L    # unfished recruitment (Ro is scaled; use relative comparisons)
nsim <- 10000L   # Monte Carlo replicates per scenario × growth × U combination

# ── Regulation scenario parameters ───────────────────────────────────────────

# Scenario 1: Minimum length 254 mm (10 in.)
# Most common state minimum for white crappie. Status quo benchmark.
scen1_name        <- "Min. length 254 mm (10 in.)"
scen1_Harvlim     <- 254
scen1_enable_slot <- FALSE
scen1_slot_type   <- "traditional"   # slot args are ignored when enable_slot=FALSE
scen1_slot_upper  <- NA_real_
scen1_DisMort     <- 0.09            # 9% release mortality (Smith et al. 2025)

# Scenario 2: Minimum length 305 mm (12 in.)
# More restrictive alternative; lets sub-quality fish survive to trophy size.
scen2_name        <- "Min. length 305 mm (12 in.)"
scen2_Harvlim     <- 305
scen2_enable_slot <- FALSE
scen2_slot_type   <- "traditional"
scen2_slot_upper  <- NA_real_
scen2_DisMort     <- 0.09

# Scenario 3: Maximum length limit 356 mm (14 in.)
# Fish above 14" must be released; only sub-memorable fish are harvestable.
# Illustrates growth overfishing — large, highly fecund fish are protected
# but the harvestable window is narrow, so high exploitation of sub-quality
# fish can suppress YPR and proportional stock density.
scen3_name            <- "Max. length 356 mm (14 in.)"
scen3_Harvlim         <- 254            # 10 in. minimum still applies
scen3_enable_slot     <- FALSE
scen3_slot_type       <- "traditional"
scen3_slot_upper      <- NA_real_
scen3_enable_max      <- TRUE
scen3_max_size        <- 356            # 14 in. = 355.6 mm → 356 mm
scen3_DisMort         <- 0.09

# ── Scenario table ────────────────────────────────────────────────────────────
scen_params <- data.frame(
  scenario         = c(scen1_name,        scen2_name,        scen3_name),
  Harvlim          = c(scen1_Harvlim,     scen2_Harvlim,     scen3_Harvlim),
  enable_slot      = c(scen1_enable_slot, scen2_enable_slot, scen3_enable_slot),
  slot_type        = c(scen1_slot_type,   scen2_slot_type,   scen3_slot_type),
  slot_upper       = c(scen1_slot_upper,  scen2_slot_upper,  scen3_slot_upper),
  enable_max_limit = c(FALSE,             FALSE,             scen3_enable_max),
  max_harvest_size = c(NA_real_,          NA_real_,          scen3_max_size),
  DisMort          = c(scen1_DisMort,     scen2_DisMort,     scen3_DisMort),
  stringsAsFactors = FALSE
)

growth_labels <- c("slow", "moderate", "fast")

# Full crossing: 3 scenarios × 3 growth × 3 U = 27 combinations
combos <- merge(
  merge(scen_params,
        data.frame(growth_preset = growth_labels, stringsAsFactors = FALSE),
        by = character()),
  U_df,
  by = character()
)
combos <- combos[order(combos$scenario, combos$growth_preset, combos$U), ]
rownames(combos) <- NULL

n_combos <- nrow(combos)  # 27

cat("White crappie simulation\n")
cat("  Combinations :", n_combos, "(3 scenarios x 3 growth x 3 U)\n")
cat("  Replicates   :", nsim, "per combination\n")
cat("  Total ticks  :", n_combos * nsim, "\n\n")

# ── Main simulation loop ──────────────────────────────────────────────────────
pbar <- pbar_init(n_combos * nsim)

results_list <- vector("list", n_combos)

for (i in seq_len(n_combos)) {

  combo <- combos[i, ]

  # Select growth parameters for this iteration
  growth <- switch(combo$growth_preset,
    slow     = growth_slow,
    moderate = growth_moderate,
    fast     = growth_fast
  )

  # ── Step 1: Build length bins based on L-infinity ─────────────────────────
  # bin_width defaults to 10 mm; spans 0 to 1.2 × Linf
  bins <- make_length_bins(Linf = growth$linf)

  # ── Step 2: Growth transition matrix + age-1 recruit distribution ─────────
  # Growth_matrix[i,j] = prob. fish in bin i grows into bin j next year
  # recruit_dist = probability mass over length bins for age-1 recruits
  gmat <- make_growth_matrix(
    Linf          = growth$linf,
    vbk           = growth$vbk,
    t0            = growth$t0,
    bin_midpoints = bins$bin_midpoints,
    length_bins   = bins$length_bins,
    growth_cv     = 0.20
  )

  # ── Step 3: Vulnerability and life-history curves ─────────────────────────
  # Returns: Vulcap_bins (capture), Vulharv_bins (harvest), trophyvul_bins,
  #          Fec_bins (fecundity), Wt_bins (weight kg), S_bins (annual survival)
  vc <- make_vulnerability_curves(
    bin_midpoints    = bins$bin_midpoints,
    Capsize          = sp$capsize,
    Harvlim          = combo$Harvlim,
    mat_size         = sp$mat_size,
    memorable_size   = sp$memorable_size,
    wl_a             = sp$wl_a,
    wl_b             = sp$wl_b,
    nat_mort         = growth$nat_mort,
    fec_exp          = sp$fec_exp,
    enable_slot      = combo$enable_slot,
    slot_type        = combo$slot_type,
    slot_upper       = if (!is.na(combo$slot_upper)) combo$slot_upper else NULL,
    enable_max_limit = isTRUE(combo$enable_max_limit),
    max_harvest_size = if (!is.na(combo$max_harvest_size)) combo$max_harvest_size else NULL
  )

  # ── Step 4: Population simulation ─────────────────────────────────────────
  # Runs nsim stochastic replicates. collect_full_output=FALSE returns only
  # the per-replicate summary data frame (faster for large nsim).
  # sim_df columns: sim, YPR, SPR, Prop, MeanLengthHarvested, Recruit
  #   SPR = stock egg production relative to the unfished equilibrium
  #         (stochastic; may exceed 1 in favourable recruitment years)
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

  # ── Step 5: Tag each replicate row with scenario metadata ─────────────────
  d               <- sim_out$sim_df
  d$species       <- "white_crappie"
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
crappie_simulations_df <- bind_rows(results_list)

cat("\nDone.\n")
cat("  crappie_simulations_df:", nrow(crappie_simulations_df), "rows,",
    ncol(crappie_simulations_df), "cols\n")
cat("  Columns:", paste(names(crappie_simulations_df), collapse = ", "), "\n")

saveRDS(crappie_simulations_df, "scripts/crappie_sim_results.rds")
cat("  Saved -> scripts/crappie_sim_results.rds\n")
