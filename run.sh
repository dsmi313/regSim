#!/usr/bin/env bash
# Run the Shiny app from the repository root.
# Requires R and the packages: shiny, dplyr, tidyr, ggplot2, plotly.

set -e

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript is not installed. Install R and try again."
  exit 1
fi

Rscript -e "shiny::runApp('app.R', host = '0.0.0.0', port = 3838, launch.browser = TRUE)"
