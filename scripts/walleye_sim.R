# scripts/walleye_sim.R
#
# Walleye regulation scenario simulations
#
# Growth parameters: FishBase median; see also:
#   Quist, M.C., Guy, C.S., Schultz, R.D., Stephen, J.L. 2003.
#   Latitudinal comparisons of walleye growth in North America and factors
#   influencing growth of walleye in Kansas reservoirs.
#   North American Journal of Fisheries Management 23:677-692.
#
#   Hansen, M.J., Bozek, M.A. 1994. Factors affecting the suitability of
#   von Bertalanffy models for estimating walleye Stizostedion vitreum growth.
#   North American Journal of Fisheries Management 14:561-572.
#
# Exploitation rates span documented walleye fisheries. Exploitation typically
# averages 20-30% (Colby et al. 1979; 17.5-26.8% at Escanaba Lake, Wisconsin);
# Haglund, Isermann, and Sass (2016, NAJFM 36:1315-1324) reported an average of
# ~34% at Escanaba Lake over several decades before harvest elimination.
# Upper range represents heavily exploited systems.
#
# DisMort = 0.10: Payer et al. (1989, NAJFM 9:188-192) reported 5-10%
# hooking mortality for walleye on artificial lures and leeches respectively.
# Reeves and Bruesewitz (2007, NAJFM 27:443-452) reported 0-12% depending
# on season and water temperature. Value of 0.10 is conservative and represents
# warm-water open-water conditions.
#
# To combine with other species:
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
# wl_a = 6.63e-6, wl_b = 3.10  (W in kg, L in mm)
# mat_size = 356 mm (~14 in.), memorable_size = 635 mm (~25 in.)
# capsize = 330 mm (50% capture vulnerability)
# rec_cv = 1.1, amax = 15, ymax = 135, fec_exp = 1.18
sp <- get_species_preset("walleye")

# ── Growth presets ────────────────────────────────────────────────────────────
# Moderate growth approximates FishBase median for walleye.
# Slow/fast bracket the range across northern-latitude walleye populations.
#   Slow:     Linf=748 mm, K=0.24 yr-1, t0=-0.66
#   Moderate: Linf=683 mm, K=0.32 yr-1, t0=-0.52  (FishBase median)
#   Fast:     Linf=615 mm, K=0.43 yr-1, t0=-0.20
growth_slow     <- get_growth_preset("walleye", "slow")
growth_moderate <- get_growth_preset("walleye", "moderate")
growth_fast     <- get_growth_preset("walleye", "fast")

# ── Exploitation rates — Colby et al. 1979; Haglund et al. 2016 ──────────────
# Prior and elevated estimates at low / moderate / high effort.
# Typical exploitation 20-30% (Colby et al. 1979; 17.5-26.8% at Escanaba Lake);
# long-term Escanaba mean ~34% (Haglund et al. 2016). Range capped at 50%.
U_df <- data.frame(
  U_label    = c("Low", "Moderate", "High"),
  U          = c(0.15,  0.30,       0.50),
  U_category = c("Low", "Moderate", "High"),
  stringsAsFactors = FALSE
)

# ── Simulation settings ───────────────────────────────────────────────────────
Ro   <- 10000L
nsim <- 10000L

# ── Regulation scenario parameters ───────────────────────────────────────────

# Scenario 1: Minimum length 356 mm (14 in.)
# Common entry-level minimum in many Midwest walleye fisheries.
scen1_name        <- "Min. length 356 mm (14 in.)"
scen1_Harvlim     <- 356
scen1_enable_slot <- FALSE
scen1_slot_type   <- "traditional"
scen1_slot_upper  <- NA_real_
scen1_DisMort     <- 0.10   # conservative; Payer et al. 1989 (5-10%), Reeves and Bruesewitz 2007 (0-12%)

# Scenario 2: Minimum length 457 mm (18 in.)
# Conservative regulation used in trophy-focused or recovering stocks.
scen2_name        <- "Min. length 457 mm (18 in.)"
scen2_Harvlim     <- 457
scen2_enable_slot <- FALSE
scen2_slot_type   <- "traditional"
scen2_slot_upper  <- NA_real_
scen2_DisMort     <- 0.10   # conservative; Payer et al. 1989 (5-10%), Reeves and Bruesewitz 2007 (0-12%)

# Scenario 3: Traditional slot 356–457 mm (14–18 in.)
# Fish within the slot (14–18") must be released; those below 14" and above
# 18" are harvestable, allowing slot fish to grow through to the upper class.
scen3_name        <- "Traditional slot 356-457 mm"
scen3_Harvlim     <- 356
scen3_enable_slot <- TRUE
scen3_slot_type   <- "traditional"
scen3_slot_upper  <- 457
scen3_DisMort     <- 0.10   # conservative; Payer et al. 1989 (5-10%), Reeves and Bruesewitz 2007 (0-12%)

# ── Scenario table ────────────────────────────────────────────────────────────
scen_params <- data.frame(
  scenario    = c(scen1_name,        scen2_name,        scen3_name),
  Harvlim     = c(scen1_Harvlim,     scen2_Harvlim,     scen3_Harvlim),
  enable_slot = c(scen1_enable_slot, scen2_enable_slot, scen3_enable_slot),
  slot_type   = c(scen1_slot_type,   scen2_slot_type,   scen3_slot_type),
  slot_upper  = c(scen1_slot_upper,  scen2_slot_upper,  scen3_slot_upper),
  DisMort     = c(scen1_DisMort,     scen2_DisMort,     scen3_DisMort),
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
      bin_midpoints  = bins_cache[[combo$growth_preset]]$bin_midpoints,
      Capsize        = sp$capsize,
      Harvlim        = combo$Harvlim,
      mat_size       = sp$mat_size,
      memorable_size = sp$memorable_size,
      wl_a           = sp$wl_a,
      wl_b           = sp$wl_b,
      nat_mort       = growth_presets_list[[combo$growth_preset]]$nat_mort,
      fec_exp        = sp$fec_exp,
      enable_slot    = combo$enable_slot,
      slot_type      = combo$slot_type,
      slot_upper     = if (!is.na(combo$slot_upper)) combo$slot_upper else NULL
    )
  }
}

cat("Walleye simulation\n")
cat("  Combinations :", n_combos, "(3 scenarios x 3 growth x 3 U)\n")
cat("  Replicates   :", nsim, "per combination\n\n")

# ── Main simulation loop ──────────────────────────────────────────────────────
results_list <- with_progress({
  # NB: do not name this `p` — with_progress() evaluates this block in the
  # caller's environment, so `p` would leak into .GlobalEnv and shadow
  # shiny::p() (the <p> tag), breaking any Shiny app launched in the same
  # session.
  prog <- progressr::progressor(steps = n_combos)
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
    d$species       <- "walleye"
    d$scenario      <- combo$scenario
    d$growth_preset <- combo$growth_preset
    d$U_label       <- combo$U_label
    d$U             <- combo$U
    d$U_category    <- combo$U_category
    d$Harvlim       <- combo$Harvlim
    d$DisMort       <- combo$DisMort

    prog()
    d
  }, future.seed = TRUE)
})

# ── Assemble and save ─────────────────────────────────────────────────────────
walleye_simulations_df <- bind_rows(results_list)

cat("\nDone.\n")
cat("  walleye_simulations_df:", nrow(walleye_simulations_df), "rows,",
    ncol(walleye_simulations_df), "cols\n")
cat("  Columns:", paste(names(walleye_simulations_df), collapse = ", "), "\n")

saveRDS(walleye_simulations_df, "scripts/walleye_sim_results.rds")
cat("  Saved -> scripts/walleye_sim_results.rds\n")
