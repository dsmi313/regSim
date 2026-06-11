# scripts/run_scenarios.R
#
# Demonstrates running regSim regulation scenarios directly from the package
# without launching the Shiny app.
#
# Usage:
#   Rscript scripts/run_scenarios.R
#   source("scripts/run_scenarios.R")

library(regSim)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# ── Species baseline: largemouth bass ─────────────────────────────────────────
sp <- get_species_preset("lmb")

# ── Shared simulation settings ────────────────────────────────────────────────
U_fixed  <- 0.30   # exploitation rate used for single-point scenarios
Ro       <- 1000L  # unfished recruitment
nsim     <- 50L    # stochastic replicates per scenario
U_sweep  <- seq(0, 0.80, by = 0.05)  # exploitation-rate grid for yield curves

# ── Scenario table ─────────────────────────────────────────────────────────────
# Columns:
#   name        – scenario label
#   Harvlim     – minimum harvest length (mm)
#   enable_slot – TRUE for slot regulations
#   slot_type   – "protective" (protect fish within slot) or "traditional"
#   slot_upper  – upper slot edge (mm); NA when enable_slot is FALSE
#   DisMort     – discard / release mortality rate (0-1)
scenarios <- data.frame(
  name        = c(
    "No regulation",
    "Minimum length (305 mm)",
    "High minimum length (381 mm)",
    "Protective slot – low release mort",
    "Protective slot – high release mort"
  ),
  Harvlim     = c(  50,  305,  381,  305,  305),
  enable_slot = c(FALSE, FALSE, FALSE, TRUE, TRUE),
  slot_type   = c(  NA,  NA,   NA, "protective", "protective"),
  slot_upper  = c(  NA,  NA,   NA,  457,  457),
  DisMort     = c(0.10, 0.10, 0.10, 0.10, 0.50),
  stringsAsFactors = FALSE
)

# ── Helper: build model inputs for a given scenario row ───────────────────────
.build_inputs <- function(Harvlim, enable_slot, slot_type, slot_upper) {
  bins <- make_length_bins(Linf = sp$linf)

  gmat <- make_growth_matrix(
    Linf          = sp$linf,
    vbk           = sp$vbk,
    t0            = sp$t0,
    bin_midpoints = bins$bin_midpoints,
    length_bins   = bins$length_bins,
    growth_cv     = 0.10
  )

  vc <- make_vulnerability_curves(
    bin_midpoints  = bins$bin_midpoints,
    Capsize        = sp$capsize,
    Harvlim        = Harvlim,
    mat_size       = sp$mat_size,
    memorable_size = sp$memorable_size,
    wl_a           = sp$wl_a,
    wl_b           = sp$wl_b,
    nat_mort       = sp$nat_mort,
    fec_exp        = sp$fec_exp,
    enable_slot    = enable_slot,
    slot_type      = if (isTRUE(enable_slot)) slot_type else "traditional",
    slot_upper     = if (isTRUE(enable_slot)) slot_upper else NULL
  )

  list(bins = bins, gmat = gmat, vc = vc)
}

# ── Helper: run one scenario, return a tidy one-row summary ──────────────────
#
# Returns:
#   scenario            – scenario label
#   harvlim_mm          – minimum harvest length setting
#   dismort_rate        – release mortality rate (input parameter)
#   yield               – total yield = mean(YPR) × mean(Recruit)
#   YPR / YPR_sd        – yield-per-recruit mean and SD across replicates
#   SPR / SPR_sd        – spawning potential ratio mean and SD
#   mean_harvested_len  – mean length of harvested fish (mm)
#   trophy_proportion   – proportion of fish ≥ memorable size, mean and SD
#   trophy_sd
#   mean_recruit        – mean equilibrium recruitment
#   release_mort_rate   – alias for dismort_rate (for clarity in plots)
run_one_scenario <- function(name, Harvlim, enable_slot, slot_type, slot_upper, DisMort) {
  inp <- .build_inputs(Harvlim, enable_slot, slot_type, slot_upper)

  sim_out <- run_population_simulation(
    bin_midpoints       = inp$bins$bin_midpoints,
    length_bins         = inp$bins$length_bins,
    Growth_matrix       = inp$gmat$Growth_matrix,
    recruit_dist        = inp$gmat$recruit_dist,
    Vulcap_bins         = inp$vc$Vulcap_bins,
    Vulharv_bins        = inp$vc$Vulharv_bins,
    trophyvul_bins      = inp$vc$trophyvul_bins,
    Fec_bins            = inp$vc$Fec_bins,
    Wt_bins             = inp$vc$Wt_bins,
    S_bins              = inp$vc$S_bins,
    Amax                = sp$amax,
    Ymax                = sp$ymax,
    Ro                  = Ro,
    rec_cv              = sp$rec_cv,
    U                   = U_fixed,
    DisMort             = DisMort,
    nsim                = nsim,
    collect_full_output = FALSE
  )

  d <- sim_out$sim_df

  data.frame(
    scenario           = name,
    harvlim_mm         = Harvlim,
    dismort_rate       = DisMort,
    yield              = mean(d$YPR) * mean(d$Recruit),
    YPR                = mean(d$YPR),
    YPR_sd             = sd(d$YPR),
    SPR                = mean(d$SPR),
    SPR_sd             = sd(d$SPR),
    mean_harvested_len = mean(d$MeanLengthHarvested),
    trophy_proportion  = mean(d$Prop),
    trophy_sd          = sd(d$Prop),
    mean_recruit       = mean(d$Recruit),
    release_mort_rate  = DisMort,
    stringsAsFactors   = FALSE
  )
}

# ── Helper: run a yield-curve sweep for one scenario ─────────────────────────
#
# Returns the data.frame from run_yield_curve() augmented with scenario label
# and dismort_rate columns.
run_one_yield_curve <- function(name, Harvlim, enable_slot, slot_type, slot_upper, DisMort) {
  inp <- .build_inputs(Harvlim, enable_slot, slot_type, slot_upper)

  curve_df <- run_yield_curve(
    bin_midpoints  = inp$bins$bin_midpoints,
    length_bins    = inp$bins$length_bins,
    Growth_matrix  = inp$gmat$Growth_matrix,
    recruit_dist   = inp$gmat$recruit_dist,
    Vulcap_bins    = inp$vc$Vulcap_bins,
    Vulharv_bins   = inp$vc$Vulharv_bins,
    trophyvul_bins = inp$vc$trophyvul_bins,
    Fec_bins       = inp$vc$Fec_bins,
    Wt_bins        = inp$vc$Wt_bins,
    S_bins         = inp$vc$S_bins,
    Amax           = sp$amax,
    Ymax           = sp$ymax,
    Ro             = Ro,
    rec_cv         = sp$rec_cv,
    DisMort        = DisMort,
    nsim           = nsim,
    U_values       = U_sweep
  )

  curve_df$scenario     <- name
  curve_df$dismort_rate <- DisMort
  curve_df
}

# ── Run all scenarios ─────────────────────────────────────────────────────────
message("Running scenario simulations (", nrow(scenarios), " scenarios) ...")
results <- bind_rows(
  pmap(scenarios, run_one_scenario)
)

message("Scenario results:")
print(results[, c("scenario", "YPR", "SPR", "mean_harvested_len",
                  "trophy_proportion", "yield", "release_mort_rate")])

# ── Run yield curves ──────────────────────────────────────────────────────────
message("\nRunning yield curves (", nrow(scenarios), " scenarios × ",
        length(U_sweep), " U values) ...")
curves <- bind_rows(
  pmap(scenarios, run_one_yield_curve)
)

# ── Plots: scenario summaries ─────────────────────────────────────────────────

# Factor with scenarios in their defined order
scen_order <- scenarios$name
results$scenario <- factor(results$scenario, levels = scen_order)
curves$scenario  <- factor(curves$scenario,  levels = scen_order)

# 1. YPR bar chart
p_ypr <- ggplot(results,
                aes(x = scenario, y = YPR, fill = scenario)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = YPR - YPR_sd, ymax = YPR + YPR_sd),
                width = 0.3) +
  coord_flip() +
  labs(
    title    = "Yield per Recruit by Regulation Scenario (LMB)",
    subtitle = paste0("U = ", U_fixed, ", nsim = ", nsim),
    x        = NULL,
    y        = "YPR"
  ) +
  theme_minimal(base_size = 12)

# 2. SPR bar chart with 30 % reference line
p_spr <- ggplot(results,
                aes(x = scenario, y = SPR, fill = scenario)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = SPR - SPR_sd, ymax = SPR + SPR_sd),
                width = 0.3) +
  geom_hline(yintercept = 0.30, linetype = "dashed", colour = "firebrick",
             linewidth = 0.8) +
  coord_flip() +
  labs(
    title   = "Spawning Potential Ratio by Regulation Scenario",
    x       = NULL,
    y       = "SPR",
    caption = "Dashed line = 30% SPR reference"
  ) +
  theme_minimal(base_size = 12)

# 3. Trophy proportion vs. SPR trade-off scatter
p_tradeoff <- ggplot(results,
                     aes(x = SPR, y = trophy_proportion,
                         colour = scenario, shape = scenario)) +
  geom_vline(xintercept = 0.30, linetype = "dashed", colour = "firebrick",
             linewidth = 0.6) +
  geom_point(size = 5) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  labs(
    title   = "Trophy Proportion vs. SPR Trade-off",
    x       = "Spawning Potential Ratio (SPR)",
    y       = "Trophy proportion (≥ memorable size)",
    colour  = "Scenario",
    shape   = "Scenario",
    caption = "Dashed line = 30% SPR reference"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.direction = "vertical")

# 4. Summary metrics faceted bar chart (pivot to long form)
summary_long <- results |>
  select(scenario, YPR, SPR, trophy_proportion, mean_harvested_len) |>
  pivot_longer(-scenario, names_to = "metric", values_to = "value") |>
  mutate(metric = recode(metric,
    "YPR"                = "Yield per Recruit",
    "SPR"                = "Spawning Potential Ratio",
    "trophy_proportion"  = "Trophy Proportion",
    "mean_harvested_len" = "Mean Harvested Length (mm)"
  ))

p_summary <- ggplot(summary_long,
                    aes(x = scenario, y = value, fill = scenario)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~metric, scales = "free_x", ncol = 2) +
  labs(
    title = "Regulation Scenario Comparison – Key Metrics",
    x     = NULL,
    y     = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

# ── Plots: yield curves ────────────────────────────────────────────────────────

# 5. YPR yield curves
p_curve_ypr <- ggplot(curves,
                      aes(x = U, y = YPR_mean,
                          colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = YPR_mean - YPR_sd,
                  ymax = YPR_mean + YPR_sd),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = U_fixed, linetype = "dotted",
             colour = "grey30", linewidth = 0.7) +
  labs(
    title   = "Yield-per-Recruit Curves by Regulation Scenario",
    x       = "Exploitation rate (U)",
    y       = "YPR (mean ± 1 SD)",
    colour  = "Scenario",
    fill    = "Scenario",
    caption = "Dotted line = current U"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.direction = "vertical")

# 6. SPR curves vs. U with 30 % reference
p_curve_spr <- ggplot(curves,
                      aes(x = U, y = SPR_mean,
                          colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = SPR_mean - SPR_sd,
                  ymax = SPR_mean + SPR_sd),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0.30, linetype = "dashed", colour = "firebrick",
             linewidth = 0.8) +
  geom_vline(xintercept = U_fixed, linetype = "dotted",
             colour = "grey30", linewidth = 0.7) +
  labs(
    title   = "SPR Curves by Regulation Scenario",
    x       = "Exploitation rate (U)",
    y       = "SPR (mean ± 1 SD)",
    colour  = "Scenario",
    fill    = "Scenario",
    caption = "Dashed = 30% SPR reference | Dotted = current U"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.direction = "vertical")

# 7. Trophy-proportion curves vs. U
p_curve_trophy <- ggplot(curves,
                         aes(x = U, y = Prop_mean,
                             colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = Prop_mean - Prop_sd,
                  ymax = Prop_mean + Prop_sd),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = U_fixed, linetype = "dotted",
             colour = "grey30", linewidth = 0.7) +
  labs(
    title   = "Trophy Proportion Curves by Regulation Scenario",
    x       = "Exploitation rate (U)",
    y       = "Trophy proportion (mean ± 1 SD)",
    colour  = "Scenario",
    fill    = "Scenario",
    caption = "Dotted line = current U"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.direction = "vertical")

# ── Print all plots ────────────────────────────────────────────────────────────
print(p_ypr)
print(p_spr)
print(p_tradeoff)
print(p_summary)
print(p_curve_ypr)
print(p_curve_spr)
print(p_curve_trophy)

message("\nDone. Objects available in workspace: results, curves, ",
        "p_ypr, p_spr, p_tradeoff, p_summary, ",
        "p_curve_ypr, p_curve_spr, p_curve_trophy")
