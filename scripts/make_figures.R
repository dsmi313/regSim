# scripts/make_figures.R
#
# Turn the saved simulation .rds files in scripts/ into figure files.
#
# Reads:  scripts/crappie_sim_results.rds
#         scripts/walleye_sim_results.rds
#         scripts/lmb_sim_results.rds
# Writes: scripts/figures/<species>_<plot>.png   (one PNG per figure)
#         scripts/figures/all_figures.pdf         (everything, one file)
#
# Run from the repository root, either way:
#   Rscript scripts/make_figures.R
#   # or, in an R session:
#   source("scripts/make_figures.R")
#
# Any species whose .rds is missing is simply skipped (with a message).

# Build every plot object. plot_simulations.R defines make_species_plots(),
# loads the .rds files, and assembles the `plots` list. It also prints the
# figures to the active graphics device, so we park that output in a throwaway
# PDF and discard it — we write the real files ourselves below.
grDevices::pdf(tempfile(fileext = ".pdf"))
source("scripts/plot_simulations.R", local = TRUE)
grDevices::dev.off()

if (length(plots) == 0) {
  stop("No figures were built. Are the *_sim_results.rds files in scripts/?")
}

out_dir <- "scripts/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# width/height (inches) per figure
plot_dims <- list(
  p_spr        = c(8, 4), p_ypr        = c(8, 4), p_trophy     = c(8, 4),
  p_violin_spr = c(8, 5), p_violin_ypr = c(8, 5),
  p_heat       = c(8, 4), p_tradeoff   = c(6, 5)
)

# One combined PDF with every figure across every species.
grDevices::pdf(file.path(out_dir, "all_figures.pdf"),
               width = 9, height = 5.2, onefile = TRUE)
for (sp in names(plots)) {
  for (nm in names(plot_dims)) print(plots[[sp]][[nm]])
}
grDevices::dev.off()

# One PNG per figure.
for (sp in names(plots)) {
  for (nm in names(plot_dims)) {
    d     <- plot_dims[[nm]]
    fname <- file.path(out_dir, sprintf("%s_%s.png", sp, sub("^p_", "", nm)))
    ggplot2::ggsave(fname, plots[[sp]][[nm]],
                    width = d[1], height = d[2], dpi = 300)
    cat("  wrote ->", fname, "\n")
  }
}

cat("\nDone. Figures written to:", normalizePath(out_dir), "\n")
