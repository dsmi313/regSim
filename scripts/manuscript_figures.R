# scripts/manuscript_figures.R
#
# Builds manuscript Figures 1-6 (violin and line figures for White Crappie,
# Walleye and Largemouth Bass, at growth CV = main_cv) and Supplementary Table S1
# (all growth CVs) from the .rds files written by crappie_sim.R, walleye_sim.R
# and lmb_sim.R. Run from the repo root.

library(dplyr)
library(ggplot2)
library(cowplot)

# Growth CV (length at age) shown in Figures 1-6. The sim scripts also run 0.10
# and 0.20; those appear only in Supplementary Table S1 as a sensitivity check.
main_cv <- 0.15

crappie <- readRDS("scripts/crappie_sim_results.rds") |>
  filter(growth_cv == main_cv) |>
  mutate(scenario = recode(scenario,
                           "No regulation"               = "No regulation",
                           "Min. length 254 mm (10 in.)" = "Min. 254 mm",
                           "Max. length 305 mm (12 in.)" = "Max. 305 mm",
                           "Slot 254-356 mm (10-14 in.)" = "Slot 254-356 mm"
  ))

walleye <- readRDS("scripts/walleye_sim_results.rds") |>
  filter(growth_cv == main_cv) |>
  mutate(scenario = recode(scenario,
                           "Min. length 356 mm (14 in.)" = "Min. 356 mm",
                           "Min. length 457 mm (18 in.)" = "Min. 457 mm",
                           "Harvest slot 356-457 mm"     = "Slot 356-457 mm"
  ))

lmb <- readRDS("scripts/lmb_sim_results.rds") |>
  filter(growth_cv == main_cv) |>
  mutate(scenario = recode(scenario,
                           "Min. length 305 mm, low mortality"          = "Min. 305 mm, low D",
                           "Min. length 305 mm, high mortality"         = "Min. 305 mm, high D",
                           "Protective slot 305-508 mm, low mortality"  = "Slot 305-508 mm, low D",
                           "Protective slot 305-508 mm, high mortality" = "Slot 305-508 mm, high D"
  ))

growth_colors <- c(slow = "#4575b4", moderate = "#d73027", fast = "#1a9641")
growth_types  <- c(slow = "dotted",  moderate = "solid",   fast = "dashed")
growth_levels <- c("slow", "moderate", "fast")

dir.create("figures", showWarnings = FALSE)

make_line_figure <- function(df, species_label, scen_levels) {
  
  summary_df <- df |>
    mutate(
      growth_preset = factor(growth_preset, levels = growth_levels),
      scenario      = factor(scenario, levels = scen_levels)
    ) |>
    group_by(scenario, growth_preset, U) |>
    summarise(
      SPR_mean  = mean(SPR),  SPR_lo  = quantile(SPR,  0.025), SPR_hi  = quantile(SPR,  0.975),
      YPR_mean  = mean(YPR),  YPR_lo  = quantile(YPR,  0.025), YPR_hi  = quantile(YPR,  0.975),
      Prop_mean = mean(Prop), Prop_lo = quantile(Prop, 0.025), Prop_hi = quantile(Prop, 0.975),
      .groups = "drop"
    )
  
  base_theme <- theme_bw(base_size = 14) +
    theme(
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      axis.title.x     = element_blank(),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank()
    )
  
  p_spr <- ggplot(summary_df, aes(x = U, colour = growth_preset,
                                  linetype = growth_preset, fill = growth_preset)) +
    geom_ribbon(aes(ymin = SPR_lo, ymax = SPR_hi), alpha = 0.12, colour = NA) +
    geom_line(aes(y = SPR_mean), linewidth = 0.8) +
    geom_hline(yintercept = 0.30, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
    facet_wrap(~ scenario, nrow = 1) +
    scale_colour_manual(values = growth_colors) +
    scale_fill_manual(values = growth_colors) +
    scale_linetype_manual(values = growth_types) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    labs(y = "SPR") +
    base_theme
  
  p_ypr <- ggplot(summary_df, aes(x = U, colour = growth_preset,
                                  linetype = growth_preset, fill = growth_preset)) +
    geom_ribbon(aes(ymin = YPR_lo, ymax = YPR_hi), alpha = 0.12, colour = NA) +
    geom_line(aes(y = YPR_mean), linewidth = 0.8) +
    facet_wrap(~ scenario, nrow = 1) +
    scale_colour_manual(values = growth_colors) +
    scale_fill_manual(values = growth_colors) +
    scale_linetype_manual(values = growth_types) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(y = "YPR (kg)") +
    base_theme +
    theme(strip.text = element_blank())
  
  p_prop <- ggplot(summary_df, aes(x = U, colour = growth_preset,
                                   linetype = growth_preset, fill = growth_preset)) +
    geom_ribbon(aes(ymin = Prop_lo, ymax = Prop_hi), alpha = 0.12, colour = NA) +
    geom_line(aes(y = Prop_mean), linewidth = 0.8) +
    facet_wrap(~ scenario, nrow = 1) +
    scale_colour_manual(values = growth_colors, name = "Growth",
                        labels = c("Slow", "Moderate", "Fast")) +
    scale_fill_manual(values = growth_colors, name = "Growth",
                      labels = c("Slow", "Moderate", "Fast")) +
    scale_linetype_manual(values = growth_types, name = "Growth",
                          labels = c("Slow", "Moderate", "Fast")) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
    labs(y = "Prop. memorable", x = "Exploitation rate (U)") +
    base_theme +
    theme(
      legend.position = "bottom",
      axis.title.x    = element_text(margin = margin(t = 10)),
      axis.text.x     = element_text(angle = 45, hjust = 1, margin = margin(t = 4)),
      axis.ticks.x    = element_line(),
      strip.text      = element_blank(),
      plot.margin     = margin(0, 5, 10, 5)
    )
  
  title <- ggdraw() +
    draw_label(species_label, fontface = "bold", size = 13, x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  body <- plot_grid(p_spr, p_ypr, p_prop, ncol = 1,
                    rel_heights = c(1, 1, 1.6),
                    align = "v", axis = "lr")
  
  plot_grid(title, body, ncol = 1, rel_heights = c(0.03, 1))
}

make_violin_figure <- function(df, species_label, scen_levels, target_U,
                               ypr_from_zero = FALSE) {
  
  violin_df <- df |>
    dplyr::filter(growth_preset == "moderate", U %in% target_U) |>
    dplyr::mutate(
      scenario = factor(scenario, levels = scen_levels),
      U_label  = factor(paste0("U = ", scales::percent(U)),
                        levels = paste0("U = ", scales::percent(sort(target_U))))
    )
  
  base_theme <- theme_bw(base_size = 14) +
    theme(
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      axis.text.y      = element_blank(),
      axis.ticks.y     = element_blank(),
      axis.title.y     = element_blank()
    )
  
  scen_colors <- setNames(
    colorRampPalette(c("#1b7837", "#762a83", "#d6604d", "#4393c3"))(length(scen_levels)),
    scen_levels
  )
  
  # from_zero = TRUE anchors the axis at 0 (SPR, so the 0.30 threshold reads in
  # context). Otherwise the axis fits the data, still shared across the U rows.
  make_vp <- function(metric, xlab, hline = NULL, pct = FALSE, from_zero = FALSE) {
    p <- ggplot(violin_df, aes(y = scenario, x = .data[[metric]], fill = scenario)) +
      geom_violin(alpha = 0.6, linewidth = 0.3, trim = TRUE) +
      stat_summary(fun = median,
                   fun.min = function(x) quantile(x, 0.025),
                   fun.max = function(x) quantile(x, 0.975),
                   geom = "pointrange", colour = "black",
                   size = 0.3, linewidth = 0.5) +
      facet_wrap(~ U_label, ncol = 1, scales = "fixed") +
      scale_fill_manual(values = scen_colors) +
      labs(x = xlab) +
      base_theme
    if (!is.null(hline))
      p <- p + geom_vline(xintercept = hline, linetype = "dashed",
                          colour = "grey40", linewidth = 0.5)
    lims <- if (from_zero) c(0, NA) else NULL
    if (pct)
      p <- p + scale_x_continuous(labels = scales::percent_format(), limits = lims)
    else
      p <- p + scale_x_continuous(limits = lims)
    p
  }
  
  p_spr  <- make_vp("SPR",  "SPR",            hline = 0.30, pct = TRUE, from_zero = TRUE) +
    theme(axis.text.y = element_text())
  p_ypr  <- make_vp("YPR",  "YPR (kg)", from_zero = ypr_from_zero)
  p_prop <- make_vp("Prop", "Prop. memorable", pct = TRUE)
  
  title <- ggdraw() +
    draw_label(species_label, fontface = "bold", size = 13, x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  body <- plot_grid(p_spr, p_ypr, p_prop, ncol = 3,
                    rel_widths = c(1.4, 1, 1),
                    align = "h", axis = "tb")
  
  plot_grid(title, body, ncol = 1, rel_heights = c(0.03, 1))
}

line_crappie <- make_line_figure(crappie, "White Crappie",
                                 c("No regulation", "Min. 254 mm", "Max. 305 mm", "Slot 254-356 mm"))
line_walleye <- make_line_figure(walleye, "Walleye",
                                 c("Min. 356 mm", "Min. 457 mm", "Slot 356-457 mm"))
line_lmb <- make_line_figure(lmb, "Largemouth Bass",
                             c("Min. 305 mm, low D", "Min. 305 mm, high D",
                               "Slot 305-508 mm, low D", "Slot 305-508 mm, high D"))

violin_crappie <- make_violin_figure(crappie, "White Crappie",
                                     c("No regulation", "Min. 254 mm", "Max. 305 mm", "Slot 254-356 mm"),
                                     target_U = c(0.30, 0.50, 0.70))
violin_walleye <- make_violin_figure(walleye, "Walleye",
                                     c("Min. 356 mm", "Min. 457 mm", "Slot 356-457 mm"),
                                     target_U = c(0.15, 0.30, 0.50),
                                     ypr_from_zero = TRUE)
violin_lmb <- make_violin_figure(lmb, "Largemouth Bass",
                                 c("Min. 305 mm, low D", "Min. 305 mm, high D",
                                   "Slot 305-508 mm, low D", "Slot 305-508 mm, high D"),
                                 target_U = c(0.10, 0.25, 0.50))

ggsave("figures/line_crappie.png",   line_crappie,   width = 10, height = 7, dpi = 300)
ggsave("figures/line_walleye.png",   line_walleye,   width = 9,  height = 7, dpi = 300)
ggsave("figures/line_lmb.png",       line_lmb,       width = 12, height = 7, dpi = 300)
ggsave("figures/violin_crappie.png", violin_crappie, width = 10, height = 9, dpi = 300)
ggsave("figures/violin_walleye.png", violin_walleye, width = 10, height = 7, dpi = 300)
ggsave("figures/violin_lmb.png",     violin_lmb,     width = 14, height = 9, dpi = 300)


crappie <- readRDS("scripts/crappie_sim_results.rds")
walleye <- readRDS("scripts/walleye_sim_results.rds")
lmb     <- readRDS("scripts/lmb_sim_results.rds")

bind_rows(crappie, walleye, lmb) |>
  group_by(species, scenario, growth_preset, growth_cv, U, DisMort) |>
  # Intervals are the 2.5% and 97.5% quantiles across replicates, matching
  # the ribbons in the line figures. SDs are kept for reference.
  summarise(
    YPR_mean  = mean(YPR),
    YPR_q025  = quantile(YPR,  0.025),
    YPR_q975  = quantile(YPR,  0.975),
    YPR_sd    = sd(YPR),
    SPR_mean  = mean(SPR),
    SPR_q025  = quantile(SPR,  0.025),
    SPR_q975  = quantile(SPR,  0.975),
    SPR_sd    = sd(SPR),
    Prop_mean = mean(Prop),
    Prop_q025 = quantile(Prop, 0.025),
    Prop_q975 = quantile(Prop, 0.975),
    Prop_sd   = sd(Prop),
    n         = n(),
    .groups   = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 4))) |>
  write.csv("supplementary_table_S1.csv", row.names = FALSE)
