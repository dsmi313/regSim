# scripts/plot_simulations.R
#
# ggplot2 visualization code for regSim simulation outputs.
# Works with crappie_simulations_df, walleye_simulations_df, or lmb_simulations_df,
# or any bind_rows() combination of them.
#
# Workflow:
#   1. Load a species result (run the sim script OR readRDS)
#   2. Set sim_df at the top of Section 1
#   3. Set scen_levels to match your species' regulation scenario names
#   4. Source this file — six named plot objects are created
#   5. Customize and save as needed

library(dplyr)
library(tidyr)
library(ggplot2)

# ===========================================================================
# SECTION 1 — DATA INPUT
# Change sim_df and scen_levels to match the species you ran.
# ===========================================================================

# --- Pick your data -------------------------------------------------------
# Option A: load from saved .rds (fastest after an initial run)
sim_df <- readRDS("scripts/crappie_sim_results.rds")

# Option B: use an object already in your environment
# sim_df <- crappie_simulations_df
# sim_df <- walleye_simulations_df
# sim_df <- lmb_simulations_df

# Option C: combine all three species into one frame
# sim_df <- bind_rows(
#   readRDS("scripts/crappie_sim_results.rds"),
#   readRDS("scripts/walleye_sim_results.rds"),
#   readRDS("scripts/lmb_sim_results.rds")
# )

# --- Scenario order (controls legend / axis ordering) --------------------
# Change these to match your scenario names exactly (copy from your sim script).
# Crappie:
scen_levels <- c(
  "Min. length 254 mm (10 in.)",
  "Min. length 305 mm (12 in.)",
  "Protective slot 254-305 mm"
)
# Walleye (swap the block above for this one):
# scen_levels <- c(
#   "Min. length 356 mm (14 in.)",
#   "Min. length 457 mm (18 in.)",
#   "Traditional slot 356-457 mm"
# )
# LMB:
# scen_levels <- c(
#   "Min. length 305 mm, low mortality",
#   "Min. length 305 mm, high mortality",
#   "Protective slot 305-508 mm, low mortality",
#   "Protective slot 305-508 mm, high mortality"
# )

# --- Scenario colors ------------------------------------------------------
# One named color per scenario; names must match scen_levels exactly.
# Swap hex codes to match your journal's color requirements.

# Crappie (active):
scen_colors <- c(
  "Min. length 254 mm (10 in.)"    = "#1b7837",
  "Min. length 305 mm (12 in.)"    = "#762a83",
  "Protective slot 254-305 mm"     = "#d6604d"
)

# Walleye (swap above for this):
# scen_colors <- c(
#   "Min. length 356 mm (14 in.)"  = "#1b7837",
#   "Min. length 457 mm (18 in.)"  = "#762a83",
#   "Traditional slot 356-457 mm"  = "#d6604d"
# )

# LMB — 2×2: hue = regulation type, shade = mortality level (swap above):
# scen_colors <- c(
#   "Min. length 305 mm, low mortality"          = "#1b7837",
#   "Min. length 305 mm, high mortality"         = "#a6d96a",
#   "Protective slot 305-508 mm, low mortality"  = "#762a83",
#   "Protective slot 305-508 mm, high mortality" = "#c994c7"
# )

# ===========================================================================
# SECTION 2 — FACTOR ORDERING
# Sets display order for facets and axes. Adjust if you added scenarios.
# ===========================================================================

sim_df <- sim_df |>
  mutate(
    scenario      = factor(scenario,      levels = scen_levels),
    growth_preset = factor(growth_preset, levels = c("slow", "moderate", "fast")),
    U_category    = factor(U_category,    levels = c("Low", "Moderate", "High"))
  )

# ===========================================================================
# SECTION 3 — SUMMARY TABLE
# Collapses 10,000 replicates per (scenario × growth × U) down to mean,
# median, IQR, and 5th/95th percentiles. Extend with any metric you need.
# ===========================================================================

summary_df <- sim_df |>
  group_by(scenario, growth_preset, U, U_label, U_category) |>
  summarise(
    n          = n(),
    # SPR
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
    # Mean harvested length
    MHL_mean   = mean(MeanLengthHarvested),
    MHL_lo     = quantile(MeanLengthHarvested, 0.25),
    MHL_hi     = quantile(MeanLengthHarvested, 0.75),
    .groups    = "drop"
  )

# ===========================================================================
# SECTION 4 — PLOTS
# Each plot is a named object (p_spr, p_ypr, etc.). Modify theme(), labs(),
# scale_*(), and facet_*() calls to customize. Print or ggsave any of them.
# ===========================================================================

# ── Figure 1: SPR vs. exploitation rate ────────────────────────────────────
# Line = mean; ribbon = IQR (25–75th pct). Faceted by growth preset.
# The 30% SPR reference line is the most common fisheries management threshold.
p_spr <- ggplot(summary_df,
                aes(x = U, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = SPR_lo, ymax = SPR_hi),
              alpha = 0.20, colour = NA) +
  geom_line(aes(y = SPR_mean), linewidth = 0.9) +
  geom_hline(yintercept = 0.30, linetype = "dashed",
             colour = "grey40", linewidth = 0.6) +
  facet_wrap(~ growth_preset, ncol = 3,
             labeller = labeller(growth_preset = c(
               slow = "Slow growth", moderate = "Moderate growth", fast = "Fast growth"
             ))) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_colour_manual(values = scen_colors, name = "Regulation") +
  scale_fill_manual(  values = scen_colors, name = "Regulation") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format()) +
  labs(
    x       = "Exploitation rate (U)",
    y       = "Spawning potential ratio (SPR)",
    caption = "Ribbon = interquartile range across 10,000 replicates. Dashed line = 30% SPR."
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# ── Figure 2: YPR vs. exploitation rate ────────────────────────────────────
p_ypr <- ggplot(summary_df,
                aes(x = U, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = YPR_lo, ymax = YPR_hi),
              alpha = 0.20, colour = NA) +
  geom_line(aes(y = YPR_mean), linewidth = 0.9) +
  facet_wrap(~ growth_preset, ncol = 3,
             labeller = labeller(growth_preset = c(
               slow = "Slow growth", moderate = "Moderate growth", fast = "Fast growth"
             ))) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_colour_manual(values = scen_colors, name = "Regulation") +
  scale_fill_manual(  values = scen_colors, name = "Regulation") +
  labs(
    x = "Exploitation rate (U)",
    y = "Yield per recruit (YPR)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# ── Figure 3: Trophy proportion vs. exploitation rate ──────────────────────
p_trophy <- ggplot(summary_df,
                   aes(x = U, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = Prop_lo, ymax = Prop_hi),
              alpha = 0.20, colour = NA) +
  geom_line(aes(y = Prop_mean), linewidth = 0.9) +
  facet_wrap(~ growth_preset, ncol = 3,
             labeller = labeller(growth_preset = c(
               slow = "Slow growth", moderate = "Moderate growth", fast = "Fast growth"
             ))) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_colour_manual(values = scen_colors, name = "Regulation") +
  scale_fill_manual(  values = scen_colors, name = "Regulation") +
  labs(
    x = "Exploitation rate (U)",
    y = "Proportion >= memorable size"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# ── Figure 4: SPR distribution violins at target exploitation rates ─────────
# Shows the full replicate-level uncertainty (not just summary stats).
# Uses moderate growth; change growth_filter to "slow" or "fast" as needed.
# Change target_U to whatever U values matter most for your manuscript.
growth_filter <- "moderate"
target_U      <- c(0.30, 0.50, 0.70)   # must be values present in sim_df$U

violin_df <- sim_df |>
  filter(U %in% target_U, growth_preset == growth_filter) |>
  mutate(U_facet = factor(paste0("U = ", U), levels = paste0("U = ", sort(target_U))))

p_violin_spr <- ggplot(violin_df,
                       aes(x = scenario, y = SPR,
                           fill = scenario, colour = scenario)) +
  geom_violin(alpha = 0.35, linewidth = 0.3, trim = TRUE) +
  geom_boxplot(width = 0.08, alpha = 0.80, outlier.shape = NA,
               colour = "grey20", linewidth = 0.4) +
  geom_hline(yintercept = 0.30, linetype = "dashed",
             colour = "grey40", linewidth = 0.6) +
  facet_wrap(~ U_facet, ncol = 3) +
  scale_fill_manual(  values = scen_colors, name = "Regulation") +
  scale_colour_manual(values = scen_colors, name = "Regulation") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x        = NULL,
    y        = "Spawning potential ratio (SPR)",
    subtitle = paste0("Growth preset: ", growth_filter,
                      " | Violin = 10,000 replicates | Box = IQR | Dashed = 30% SPR")
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# ── Figure 4b: YPR distribution violins at target exploitation rates ────────
# Same filtering as p_violin_spr — reuses violin_df, growth_filter, target_U.
# Shows whether the yield payoff of each regulation differs across U levels.
p_violin_ypr <- ggplot(violin_df,
                       aes(x = scenario, y = YPR,
                           fill = scenario, colour = scenario)) +
  geom_violin(alpha = 0.35, linewidth = 0.3, trim = TRUE) +
  geom_boxplot(width = 0.08, alpha = 0.80, outlier.shape = NA,
               colour = "grey20", linewidth = 0.4) +
  facet_wrap(~ U_facet, ncol = 3) +
  scale_fill_manual(  values = scen_colors, name = "Regulation") +
  scale_colour_manual(values = scen_colors, name = "Regulation") +
  labs(
    x        = NULL,
    y        = "Yield per recruit (YPR)",
    subtitle = paste0("Growth preset: ", growth_filter,
                      " | Violin = 10,000 replicates | Box = IQR")
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# ── Figure 5: Mean SPR heat map ─────────────────────────────────────────────
# Summarized further to scenario × exploitation category × growth.
# Good as a compact overview figure or supplementary table replacement.
heat_df <- sim_df |>
  group_by(scenario, growth_preset, U_category) |>
  summarise(SPR_mean = mean(SPR), .groups = "drop")

p_heat <- ggplot(heat_df,
                 aes(x = U_category, y = scenario, fill = SPR_mean)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", SPR_mean)),
            size = 3.2, colour = "white", fontface = "bold") +
  facet_wrap(~ growth_preset, ncol = 3,
             labeller = labeller(growth_preset = c(
               slow = "Slow growth", moderate = "Moderate growth", fast = "Fast growth"
             ))) +
  scale_fill_gradient2(
    low      = "#d6604d",   # red = low SPR (overfished)
    mid      = "#f7f7f7",   # white = at reference
    high     = "#4393c3",   # blue = high SPR (protected)
    midpoint = 0.30,
    limits   = c(0, 1),
    labels   = scales::percent_format(),
    name     = "Mean SPR"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = "Exploitation category",
    y = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold"),
    axis.text.y      = element_text(size = 9),
    panel.grid       = element_blank()
  )

# ── Figure 6: YPR–SPR tradeoff scatter ──────────────────────────────────────
# Each point = one (scenario × growth × U) combination.
# Shows whether regulations shift the tradeoff curve.
p_tradeoff <- ggplot(summary_df,
                     aes(x = SPR_mean, y = YPR_mean,
                         colour = scenario, shape = growth_preset)) +
  geom_vline(xintercept = 0.30, linetype = "dashed",
             colour = "grey40", linewidth = 0.6) +
  geom_point(size = 2.8, alpha = 0.85) +
  scale_colour_manual(values = scen_colors, name = "Regulation") +
  scale_shape_manual( values = c(16, 17, 15),
                      name   = "Growth",
                      labels = c("Slow", "Moderate", "Fast")) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    x       = "Mean SPR",
    y       = "Mean YPR",
    caption = "Each point = one scenario × growth preset × U. Dashed = 30% SPR."
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    legend.box      = "vertical"
  )

# ===========================================================================
# SECTION 5 — PRINT / SAVE
# Print all six figures, then save individually.
# Adjust width/height and file extension (pdf, tiff, png) to journal specs.
# ===========================================================================

print(p_spr)
print(p_ypr)
print(p_trophy)
print(p_violin_spr)
print(p_violin_ypr)
print(p_heat)
print(p_tradeoff)

# Uncomment to save (change filename prefix, dimensions, and dpi to taste):
# ggsave("scripts/fig_spr.png",        p_spr,        width = 8, height = 4, dpi = 300)
# ggsave("scripts/fig_ypr.png",        p_ypr,        width = 8, height = 4, dpi = 300)
# ggsave("scripts/fig_trophy.png",     p_trophy,     width = 8, height = 4, dpi = 300)
# ggsave("scripts/fig_violin_spr.png", p_violin_spr, width = 8, height = 5, dpi = 300)
# ggsave("scripts/fig_violin_ypr.png", p_violin_ypr, width = 8, height = 5, dpi = 300)
# ggsave("scripts/fig_heat.png",       p_heat,       width = 8, height = 4, dpi = 300)
# ggsave("scripts/fig_tradeoff.png",   p_tradeoff,   width = 6, height = 5, dpi = 300)

cat("\nPlot objects: p_spr, p_ypr, p_trophy, p_violin_spr, p_violin_ypr, p_heat, p_tradeoff\n")
cat("Summary data: summary_df (", nrow(summary_df), "rows )\n")
