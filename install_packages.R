# Install regSim and its dependencies.
# Run with: Rscript install_packages.R
#
# PRIMARY workflow — install the package directly from GitHub:
#   install.packages("remotes")
#   remotes::install_github("dsmi313/regSim")
#   regSim::run_app()
#
# DEVELOPMENT workflow — install dependencies for running app.R directly
# from a local clone of the repository:
#   Rscript install_packages.R --dev
#   shiny::runApp("app.R")

args    <- commandArgs(trailingOnly = TRUE)
dev_mode <- "--dev" %in% args

if (!dev_mode) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }
  message("Installing regSim from GitHub...")
  remotes::install_github("dsmi313/regSim")
  message("Done. Launch the app with: regSim::run_app()")
} else {
  packages  <- c("shiny", "dplyr", "tidyr", "ggplot2", "plotly")
  installed <- rownames(installed.packages())
  missing   <- setdiff(packages, installed)
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  } else {
    message("All required packages are already installed.")
  }
  message("Done. Launch the app with: shiny::runApp(\"app.R\")")
}
