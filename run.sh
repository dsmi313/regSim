#!/usr/bin/env bash
# Launch the regSim Shiny app.
#
# Primary path  — package installed from GitHub (recommended):
#   install.packages("remotes")
#   remotes::install_github("dsmi313/regSim")
#   regSim::run_app()
#
# Development path — run from a local clone of the repository:
#   shiny::runApp("app.R")

set -e

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript is not installed. Install R and try again."
  exit 1
fi

Rscript -e "regSim::run_app(host = '0.0.0.0', port = 3838, launch.browser = TRUE)"
