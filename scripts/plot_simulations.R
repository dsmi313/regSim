# scripts/plot_simulations.R
#
# ggplot2 visualization code for regSim simulation outputs.
#
# This script builds every figure for ALL THREE species in one pass. Once you
# have run the three simulation scripts (crappie_sim.R, walleye_sim.R,
# lmb_sim.R) and saved their .rds files, just source this file — it loops over
# each species and produces a full set of named plot objects for every one.
#
# Workflow:
#   1. Run the sim scripts (or readRDS) so the three .rds files exist.
#   2. Source this file.
#   3. Plots for all species live in the `plots` list, keyed by species:
#        plots$crappie$p_violin_relegg
#        plots$walleye$p_spr
#        plots$lmb$p_tradeoff
#   4. Set save_plots <- TRUE to write every figure to scripts/ as PNG.

library(dplyr)
library(tidyr)
library(ggplot2)

# ===========================================================================
# SECTION 0 — GLOBAL OPTIONS
# ===========================================================================

# Set TRUE to write every figure for every species to scripts/ as PNG.
save_plots <- FALSE

# Shared factor orders (same across species).
growth_levels <- c("slow", "moderate", "fast")
Ucat_levels   <- c("Low", "Moderate", "High")

# ===========================================================================
# SECTION 1 — PER-SPECIES CONFIGURATION
# One entry per species. Each lists the saved .rds, the scenario order, the
# scenario colours, which growth preset the violins use, and which U values
# to show in the violins (must be values present in that species' sim_df$U).
# ===========================================================================

species_config <- list(

  # ── White crappie ───────────────────────────────────────────────────────
  crappie = list(
    label        = "White crappie",
    rds          = "scripts/crappie_sim_results.rds",
    scen_levels  = c(
      "Min. length 254 mm (10 in.)",
      "Min. length 305 mm (12 in.)",
      "Protective slot 254-305 mm"
    ),
    scen_colors  = c(
      "Min. length 254 mm (10 in.)" = "#1b7837",
      "Min. length 305 mm (12 in.)" = "#762a83",
      "Protective slot 254-305 mm"  = "#d6604d"
    ),
    growth_filter = "moderate",
    target_U      = c(0.30, 0.50, 0.70)
  ),

  # ── Walleye ─────────────────────────────────────────────────────────────
  walleye = list(
    label        = "Walleye",
    rds          = "scripts/walleye_sim_results.rds",
    scen_levels  = c(
      "Min. length 356 mm (14 in.)",
      "Min. length 457 mm (18 in.)",
      "Traditional slot 356-457 mm"
    ),
    scen_colors  = c(
      "Min. length 356 mm (14 in.)" = "#1b7837",
      "Min. length 457 mm (18 in.)" = "#762a83",
      "Traditional slot 356-457 mm" = "#d6604d"
    ),
    growth_filter = "moderate",
    target_U      = c(0.15, 0.30, 0.50)
  ),

  # ── Largemouth bass (2x2: regulation type x mortality level) ─────────────
  lmb = list(
    label        = "Largemouth bass",
    rds          = "scripts/lmb_sim_results.rds",
    scen_levels  = c(
      "Min. length 305 mm, low mortality",
      "Min. length 305 mm, high mortality",
      "Protective slot 305-508 mm, low mortality",
      "Protective slot 305-508 mm, high mortality"
    ),
    scen_colors  = c(
      "Min. length 305 mm, low mortality"          = "#1b7837",
      "Min. length 305 mm, high mortality"         = "#a6d96a",
      "Protective slot 305-508 mm, low mortality"  = "#762a83",
      "Protective slot 305-508 mm, high mortality" = "#c994c7"
    ),
    growth_filter = "moderate",
    target_U      = c(0.10, 0.25, 0.50)
  )
)

# ===========================================================================
# SECTION 2 — PLOT BUILDER
# Builds the full figure set for one species and returns them as a named list
# (plus the summary_df used by the trend / heatmap / tradeoff plots).
# ===========================================================================

make_species_plots <- function(cfg) {

  growth_lab <- c(slow = "Slow growth",
                  moderate = "Moderate growth",
                  fast = "Fast growth")

  # ---- Load + factor ordering --------------------------------------------
  sim_df <- readRDS(cfg$rds) |>
    mutate(
      scenario      = factor(scenario,      levels = cfg$scen_levels),
      growth_preset = factor(growth_preset, levels = growth_levels),
      U_category    = factor(U_category,    levels = Ucat_levels)
    )

  # ---- Summary table (mean / IQR / 5th-95th per combo) -------------------
  summary_df <- sim_df |>
    group_by(scenario, growth_preset, U, U_label, U_category) |>
    summarise(
      n          = n(),
      # SPR (deterministic per-recruit ratio)
      SPR_mean   = mean(SPR),
      SPR_med    = median(SPR),
      SPR_lo     = quantile(SPR, 0.25),
      SPR_hi     = quantile(SPR, 0.75),
      SPR_p05    = quantile(SPR, 0.05),
      SPR_p95    = quantile(SPR, 0.95),
      # YPR
      YPR_mean   = mean(YPR),
      YPR_med    = median(YPR),
      YPR_lo     = quantile(YPR, 0.25),
      YPR_hi     = quantile(YPR, 0.75),
      YPR_p05    = quantile(YPR, 0.05),
      YPR_p95    = quantile(YPR, 0.95),
      # Trophy proportion (Prop = proportion >= memorable size)
      Prop_mean  = mean(Prop),
      Prop_med   = median(Prop),
      Prop_lo    = quantile(Prop, 0.25),
      Prop_hi    = quantile(Prop, 0.75),
      # Relative egg production (stochastic; can exceed 1)
      RelEgg_mean = mean(RelEgg),
      RelEgg_med  = median(RelEgg),
      RelEgg_lo   = quantile(RelEgg, 0.25),
      RelEgg_hi   = quantile(RelEgg, 0.75),
      RelEgg_p05  = quantile(RelEgg, 0.05),
      RelEgg_p95  = quantile(RelEgg, 0.95),
      # Mean harvested length
      MHL_mean   = mean(MeanLengthHarvested),
      MHL_lo     = quantile(MeanLengthHarvested, 0.25),
      MHL_hi     = quantile(MeanLengthHarvested, 0.75),
      .groups    = "drop"
    )

  # Reusable theme bits
  base_theme <- theme_bw(base_size = 12) +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold")
    )
  violin_theme <- base_theme +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  # ── Figure 1: SPR vs. exploitation rate ─────────────────────────────────
  p_spr <- ggplot(summary_df, aes(x = U, colour = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = SPR_lo, ymax = SPR_hi), alpha = 0.20, colour = NA) +
    geom_line(aes(y = SPR_mean), linewidth = 0.9) +
    geom_hline(yintercept = 0.30, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    facet_wrap(~ growth_preset, ncol = 3,
               labeller = labeller(growth_preset = growth_lab)) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_colour_manual(values = cfg$scen_colors, name = "Regulation") +
    scale_fill_manual(  values = cfg$scen_colors, name = "Regulation") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = scales::percent_format()) +
    labs(title = cfg$label,
         x = "Exploitation rate (U)",
         y = "Spawning potential ratio (SPR)",
         caption = "Ribbon = interquartile range across replicates. Dashed = 30% SPR.") +
    base_theme

  # ── Figure 2: YPR vs. exploitation rate ─────────────────────────────────
  p_ypr <- ggplot(summary_df, aes(x = U, colour = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = YPR_lo, ymax = YPR_hi), alpha = 0.20, colour = NA) +
    geom_line(aes(y = YPR_mean), linewidth = 0.9) +
    facet_wrap(~ growth_preset, ncol = 3,
               labeller = labeller(growth_preset = growth_lab)) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_colour_manual(values = cfg$scen_colors, name = "Regulation") +
    scale_fill_manual(  values = cfg$scen_colors, name = "Regulation") +
    labs(title = cfg$label,
         x = "Exploitation rate (U)", y = "Yield per recruit (YPR)") +
    base_theme

  # ── Figure 3: Trophy proportion vs. exploitation rate ───────────────────
  p_trophy <- ggplot(summary_df, aes(x = U, colour = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = Prop_lo, ymax = Prop_hi), alpha = 0.20, colour = NA) +
    geom_line(aes(y = Prop_mean), linewidth = 0.9) +
    facet_wrap(~ growth_preset, ncol = 3,
               labeller = labeller(growth_preset = growth_lab)) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_colour_manual(values = cfg$scen_colors, name = "Regulation") +
    scale_fill_manual(  values = cfg$scen_colors, name = "Regulation") +
    labs(title = cfg$label,
         x = "Exploitation rate (U)", y = "Proportion >= memorable size") +
    base_theme

  # ── Violin data: target U levels at one growth preset ───────────────────
  violin_df <- sim_df |>
    filter(U %in% cfg$target_U, growth_preset == cfg$growth_filter) |>
    mutate(U_facet = factor(paste0("U = ", U),
                            levels = paste0("U = ", sort(cfg$target_U))))

  # ── Figure 4: SPR violins (deterministic per-recruit; tight) ────────────
  p_violin_spr <- ggplot(violin_df,
                         aes(x = scenario, y = SPR,
                             fill = scenario, colour = scenario)) +
    geom_violin(alpha = 0.35, linewidth = 0.3, trim = TRUE) +
    geom_boxplot(width = 0.08, alpha = 0.80, outlier.shape = NA,
                 colour = "grey20", linewidth = 0.4) +
    geom_hline(yintercept = 0.30, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    facet_wrap(~ U_facet, ncol = 3) +
    scale_fill_manual(  values = cfg$scen_colors, name = "Regulation") +
    scale_colour_manual(values = cfg$scen_colors, name = "Regulation") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = cfg$label, x = NULL,
         y = "Spawning potential ratio (SPR)",
         subtitle = paste0("Growth preset: ", cfg$growth_filter,
                           " | Box = IQR | Dashed = 30% SPR")) +
    violin_theme

  # ── Figure 4b: YPR violins ──────────────────────────────────────────────
  p_violin_ypr <- ggplot(violin_df,
                         aes(x = scenario, y = YPR,
                             fill = scenario, colour = scenario)) +
    geom_violin(alpha = 0.35, linewidth = 0.3, trim = TRUE) +
    geom_boxplot(width = 0.08, alpha = 0.80, outlier.shape = NA,
                 colour = "grey20", linewidth = 0.4) +
    facet_wrap(~ U_facet, ncol = 3) +
    scale_fill_manual(  values = cfg$scen_colors, name = "Regulation") +
    scale_colour_manual(values = cfg$scen_colors, name = "Regulation") +
    labs(title = cfg$label, x = NULL, y = "Yield per recruit (YPR)",
         subtitle = paste0("Growth preset: ", cfg$growth_filter,
                           " | Violin = replicates | Box = IQR")) +
    violin_theme

  # ── Figure 4c: RelEgg violins (stochastic; may exceed 1) ────────────────
  p_violin_relegg <- ggplot(violin_df,
                            aes(x = scenario, y = RelEgg,
                                fill = scenario, colour = scenario)) +
    geom_violin(alpha = 0.35, linewidth = 0.3, trim = TRUE) +
    geom_boxplot(width = 0.08, alpha = 0.80, outlier.shape = NA,
                 colour = "grey20", linewidth = 0.4) +
    geom_hline(yintercept = 1.0, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    facet_wrap(~ U_facet, ncol = 3) +
    scale_fill_manual(  values = cfg$scen_colors, name = "Regulation") +
    scale_colour_manual(values = cfg$scen_colors, name = "Regulation") +
    labs(title = cfg$label, x = NULL,
         y = "Relative egg production (RelEgg)",
         subtitle = paste0("Growth preset: ", cfg$growth_filter,
                           " | Box = IQR | Dashed = unfished reference (1.0)")) +
    violin_theme

  # ── Figure 5: Mean SPR heat map ─────────────────────────────────────────
  heat_df <- sim_df |>
    group_by(scenario, growth_preset, U_category) |>
    summarise(SPR_mean = mean(SPR), .groups = "drop")

  p_heat <- ggplot(heat_df,
                   aes(x = U_category, y = scenario, fill = SPR_mean)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%.2f", SPR_mean)),
              size = 3.2, colour = "white", fontface = "bold") +
    facet_wrap(~ growth_preset, ncol = 3,
               labeller = labeller(growth_preset = growth_lab)) +
    scale_fill_gradient2(
      low = "#d6604d", mid = "#f7f7f7", high = "#4393c3",
      midpoint = 0.30, limits = c(0, 1),
      labels = scales::percent_format(), name = "Mean SPR") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = cfg$label, x = "Exploitation category", y = NULL) +
    theme_bw(base_size = 12) +
    theme(strip.background = element_blank(),
          strip.text  = element_text(face = "bold"),
          axis.text.y = element_text(size = 9),
          panel.grid  = element_blank())

  # ── Figure 6: YPR–SPR tradeoff scatter ──────────────────────────────────
  p_tradeoff <- ggplot(summary_df,
                       aes(x = SPR_mean, y = YPR_mean,
                           colour = scenario, shape = growth_preset)) +
    geom_vline(xintercept = 0.30, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    geom_point(size = 2.8, alpha = 0.85) +
    scale_colour_manual(values = cfg$scen_colors, name = "Regulation") +
    scale_shape_manual(values = c(16, 17, 15), name = "Growth",
                       labels = c("Slow", "Moderate", "Fast")) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(title = cfg$label, x = "Mean SPR", y = "Mean YPR",
         caption = "Each point = one scenario x growth preset x U. Dashed = 30% SPR.") +
    theme_bw(base_size = 12) +
    theme(legend.position = "right", legend.box = "vertical")

  list(
    summary_df      = summary_df,
    p_spr           = p_spr,
    p_ypr           = p_ypr,
    p_trophy        = p_trophy,
    p_violin_spr    = p_violin_spr,
    p_violin_ypr    = p_violin_ypr,
    p_violin_relegg = p_violin_relegg,
    p_heat          = p_heat,
    p_tradeoff      = p_tradeoff
  )
}

# ===========================================================================
# SECTION 3 — BUILD EVERYTHING FOR EVERY SPECIES
# `plots` is a named list: plots$crappie$p_spr, plots$walleye$p_violin_relegg…
# Species whose .rds file is missing are skipped with a message.
# ===========================================================================

plots <- list()

for (sp_key in names(species_config)) {
  cfg <- species_config[[sp_key]]
  if (!file.exists(cfg$rds)) {
    cat(sprintf("  [skip] %-15s — %s not found (run its sim script first)\n",
                cfg$label, cfg$rds))
    next
  }
  cat(sprintf("  [build] %-15s from %s\n", cfg$label, cfg$rds))
  plots[[sp_key]] <- make_species_plots(cfg)
}

# ===========================================================================
# SECTION 4 — PRINT / SAVE
# Prints every figure for every species. With save_plots = TRUE, also writes
# each figure to scripts/fig_<species>_<plot>.png.
# ===========================================================================

plot_dims <- list(
  p_spr           = c(8, 4), p_ypr        = c(8, 4), p_trophy     = c(8, 4),
  p_violin_spr    = c(8, 5), p_violin_ypr = c(8, 5), p_violin_relegg = c(8, 5),
  p_heat          = c(8, 4), p_tradeoff   = c(6, 5)
)

for (sp_key in names(plots)) {
  sp_plots <- plots[[sp_key]]
  for (nm in names(plot_dims)) {
    print(sp_plots[[nm]])
    if (save_plots) {
      dims <- plot_dims[[nm]]
      fname <- sprintf("scripts/fig_%s_%s.png", sp_key, sub("^p_", "", nm))
      ggsave(fname, sp_plots[[nm]],
             width = dims[1], height = dims[2], dpi = 300)
      cat("  saved ->", fname, "\n")
    }
  }
}

cat("\nBuilt species:", paste(names(plots), collapse = ", "), "\n")
cat("Access plots via e.g. plots$crappie$p_violin_relegg\n")
cat("Summary tables via e.g. plots$crappie$summary_df\n")
