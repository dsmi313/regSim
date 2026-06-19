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
library(future.apply)
library(progressr)
library(dplyr)

plan(multicore, workers = parallelly::availableCores())
handlers(handler_progress(
  format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || ETA: :eta]",
  clear  = FALSE,
  width  = 100
))

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

# Scenario 2: Maximum length limit 305 mm (12 in.), no minimum.
# All fish up to 12" may be harvested; fish > 12" must be released.
# Illustrates growth overfishing — large fecund fish are protected but
# removing sub-quality fish before they reach full size suppresses YPR
# and egg production.
scen2_name            <- "Max. length 305 mm (12 in.)"
scen2_Harvlim         <- 0
scen2_enable_slot     <- FALSE
scen2_slot_type       <- "traditional"
scen2_slot_upper      <- NA_real_
scen2_enable_max      <- TRUE
scen2_max_size        <- 305
scen2_DisMort         <- 0.09

# Scenario 3: No regulation.
# Common in many crappie fisheries; all fish are harvestable regardless of
# size. Serves as an unregulated baseline — expect lower SPR and YPR at
# high exploitation relative to size-limit scenarios.
scen3_name        <- "No regulation"
scen3_Harvlim     <- 0
scen3_enable_slot <- FALSE
scen3_slot_type   <- "traditional"
scen3_slot_upper  <- NA_real_
scen3_DisMort     <- 0.09

# Scenario 4: Traditional slot 254-356 mm (10-14 in.)
# Fish within the 10-14" slot are harvestable; fish below 10" or above 14"
# must be released. Under live-sonar exploitation, the slot concentrates
# harvest pressure on quality-class fish while protecting the smallest and
# largest individuals. Relevant as a proposed response to live-imaging sonar.
scen4_name        <- "Slot 254-356 mm (10-14 in.)"
scen4_Harvlim     <- 254
scen4_enable_slot <- TRUE
scen4_slot_type   <- "traditional"     # fish WITHIN [Harvlim, slot_upper] are harvestable
scen4_slot_upper  <- 356              # 14 in. = 355.6 mm → 356 mm
scen4_DisMort     <- 0.09

# ── Scenario table ────────────────────────────────────────────────────────────
scen_params <- data.frame(
  scenario         = c(scen1_name,        scen2_name,        scen3_name,        scen4_name),
  Harvlim          = c(scen1_Harvlim,     scen2_Harvlim,     scen3_Harvlim,     scen4_Harvlim),
  enable_slot      = c(scen1_enable_slot, scen2_enable_slot, scen3_enable_slot, scen4_enable_slot),
  slot_type        = c(scen1_slot_type,   scen2_slot_type,   scen3_slot_type,   scen4_slot_type),
  slot_upper       = c(scen1_slot_upper,  scen2_slot_upper,  scen3_slot_upper,  scen4_slot_upper),
  enable_max_limit = c(FALSE,             scen2_enable_max,  FALSE,             FALSE),
  max_harvest_size = c(NA_real_,          scen2_max_size,    NA_real_,          NA_real_),
  DisMort          = c(scen1_DisMort,     scen2_DisMort,     scen3_DisMort,     scen4_DisMort),
  stringsAsFactors = FALSE
)

growth_labels <- c("slow", "moderate", "fast")

# Full crossing: 4 scenarios × 3 growth × 3 U = 36 combinations
combos <- merge(
  merge(scen_params,
        data.frame(growth_preset = growth_labels, stringsAsFactors = FALSE),
        by = character()),
  U_df,
  by = character()
)
combos <- combos[order(combos$scenario, combos$growth_preset, combos$U), ]
rownames(combos) <- NULL

n_combos <- nrow(combos)  # 36

# ── Pre-compute cached inputs (bins and gmat depend only on growth_preset;
#    vc depends on scenario × growth_preset but NOT on U) ─────────────────────
growth_presets_list <- list(
  slow     = growth_slow,
  moderate = growth_moderate,
  fast     = growth_fast
)

bins_cache <- vector("list", length(growth_presets_list))
names(bins_cache) <- names(growth_presets_list)
gmat_cache <- vector("list", length(growth_presets_list))
names(gmat_cache) <- names(growth_presets_list)

for (gp in names(growth_presets_list)) {
  g <- growth_presets_list[[gp]]
  bins_cache[[gp]] <- make_length_bins(Linf = g$linf)
  gmat_cache[[gp]] <- make_growth_matrix(
    Linf          = g$linf,
    vbk           = g$vbk,
    t0            = g$t0,
    bin_midpoints = bins_cache[[gp]]$bin_midpoints,
    length_bins   = bins_cache[[gp]]$length_bins,
    growth_cv     = 0.20
  )
}

vc_cache <- list()
for (i in seq_len(nrow(combos))) {
  combo <- combos[i, ]
  key   <- paste(combo$scenario, combo$growth_preset, sep = "\n")
  if (is.null(vc_cache[[key]])) {
    vc_cache[[key]] <- make_vulnerability_curves(
      bin_midpoints    = bins_cache[[combo$growth_preset]]$bin_midpoints,
      Capsize          = sp$capsize,
      Harvlim          = combo$Harvlim,
      mat_size         = sp$mat_size,
      memorable_size   = sp$memorable_size,
      wl_a             = sp$wl_a,
      wl_b             = sp$wl_b,
      nat_mort         = growth_presets_list[[combo$growth_preset]]$nat_mort,
      fec_exp          = sp$fec_exp,
      enable_slot      = combo$enable_slot,
      slot_type        = combo$slot_type,
      slot_upper       = if (!is.na(combo$slot_upper)) combo$slot_upper else NULL,
      enable_max_limit = isTRUE(combo$enable_max_limit),
      max_harvest_size = if (!is.na(combo$max_harvest_size)) combo$max_harvest_size else NULL
    )
  }
}

cat("White crappie simulation\n")
cat("  Combinations :", n_combos, "(4 scenarios x 3 growth x 3 U)\n")
cat("  Replicates   :", nsim, "per combination\n\n")

# ── Main simulation loop ──────────────────────────────────────────────────────
# sim_df columns: sim, YPR, SPR, Prop, MeanLengthHarvested, Recruit
#   SPR = stock egg production relative to the unfished equilibrium
#         (stochastic; may exceed 1 in favourable recruitment years)
results_list <- with_progress({
  p <- progressr::progressor(steps = n_combos)
  future_lapply(seq_len(n_combos), function(i) {
    combo <- combos[i, ]
    key   <- paste(combo$scenario, combo$growth_preset, sep = "\n")

    bins <- bins_cache[[combo$growth_preset]]
    gmat <- gmat_cache[[combo$growth_preset]]
    vc   <- vc_cache[[key]]

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
      collect_full_output = FALSE
    )

    d               <- sim_out$sim_df
    d$species       <- "white_crappie"
    d$scenario      <- combo$scenario
    d$growth_preset <- combo$growth_preset
    d$U_label       <- combo$U_label
    d$U             <- combo$U
    d$U_category    <- combo$U_category
    d$Harvlim       <- combo$Harvlim
    d$DisMort       <- combo$DisMort

    p()
    d
  }, future.seed = TRUE)
})

# ── Assemble and save ─────────────────────────────────────────────────────────
crappie_simulations_df <- bind_rows(results_list)

cat("\nDone.\n")
cat("  crappie_simulations_df:", nrow(crappie_simulations_df), "rows,",
    ncol(crappie_simulations_df), "cols\n")
cat("  Columns:", paste(names(crappie_simulations_df), collapse = ", "), "\n")

saveRDS(crappie_simulations_df, "scripts/crappie_sim_results.rds")
cat("  Saved -> scripts/crappie_sim_results.rds\n")
