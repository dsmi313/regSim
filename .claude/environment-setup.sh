#!/bin/bash
# Claude Code on the web — ENVIRONMENT setup script.
#
# Paste the contents of this file into your environment's "Setup script" field
# in the Claude Code web UI (Environments settings). Unlike a repo-level
# SessionStart hook, this runs on container startup for EVERY session in the
# environment, regardless of which repository is loaded — so R is always
# available in any current or future repo.
#
# Installs R plus a common set of R packages from Ubuntu apt binaries
# (no source compilation). Idempotent and non-interactive.
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive

SUDO=""
if [ "$(id -u)" -ne 0 ]; then
  SUDO="sudo"
fi

$SUDO apt-get update -qq
$SUDO apt-get install -y -qq --no-install-recommends \
  r-base-core \
  r-cran-dplyr \
  r-cran-tidyr \
  r-cran-ggplot2 \
  r-cran-plotly \
  r-cran-shiny \
  r-cran-testthat \
  r-cran-pkgload \
  r-cran-lintr \
  r-cran-future.apply \
  r-cran-progressr \
  r-cran-parallelly

echo "Environment setup: R $(Rscript -e 'cat(as.character(getRversion()))') ready."
