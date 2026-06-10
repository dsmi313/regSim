# Install required packages for the regSim Shiny app.
# Run with: Rscript install_packages.R

packages <- c("shiny", "dplyr", "tidyr", "ggplot2", "plotly")
installed <- rownames(installed.packages())
missing <- setdiff(packages, installed)

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
