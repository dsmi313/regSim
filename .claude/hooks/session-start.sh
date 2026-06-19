#!/bin/bash
# SessionStart hook: install R and the regSim package dependencies so tests,
# linters, the Shiny app, and the simulation/plot scripts run in Claude Code
# on the web sessions.
#
# Runs only in the remote (web) environment. Idempotent and non-interactive.
# All deps are installed from Ubuntu's apt binaries (no source compilation),
# which keeps startup fast and lets the cached container reuse them.
set -euo pipefail

# Only run in the remote (Claude Code on the web) environment.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

export DEBIAN_FRONTEND=noninteractive

SUDO=""
if [ "$(id -u)" -ne 0 ]; then
  SUDO="sudo"
fi

# R interpreter + regSim's runtime, simulation, and test dependencies.
# (Suggests: future.apply/progressr/parallelly power the scripts/ sims;
#  testthat + pkgload run tests/ the way CI does, via load_all();
#  lintr is the linter.)
PACKAGES=(
  r-base-core
  r-cran-dplyr
  r-cran-tidyr
  r-cran-ggplot2
  r-cran-plotly
  r-cran-shiny
  r-cran-testthat
  r-cran-pkgload
  r-cran-future.apply
  r-cran-progressr
  r-cran-parallelly
  r-cran-lintr
)

$SUDO apt-get update -qq
$SUDO apt-get install -y -qq --no-install-recommends "${PACKAGES[@]}"

echo "SessionStart hook: R $(Rscript -e 'cat(as.character(getRversion()))') ready."
