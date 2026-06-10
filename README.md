# regSim

A YPR / SPR app used to support management decisions in fisheries.

`regSim` is an interactive Shiny application for yield-per-recruit (YPR) and
spawning potential ratio (SPR) analysis in age-structured fish population
models.

## Install and run (recommended)

Install directly from GitHub as an R package and launch the app:

```r
install.packages("remotes")
remotes::install_github("dsmi313/regSim")
regSim::run_app()
```

That's it — no need to clone the repository or source any files manually. The
Shiny app and all modeling functions ship inside the package.

Requirements:
- R installed
- The package pulls in its dependencies automatically: `shiny`, `dplyr`,
  `tidyr`, `ggplot2`, `plotly`.

## Development

Contributors working from a local clone of the repository can still run the
app straight from the root `app.R` script (kept as a development backup):

```r
shiny::runApp("app.R")
```

To install missing R packages for the script-based workflow:

```bash
Rscript install_packages.R
```

Or use the helper script from the repository root:

```bash
chmod +x run.sh
./run.sh
```

## Package layout

- `inst/shiny/app.R` — the packaged Shiny app launched by `regSim::run_app()`.
- `app.R` — root development copy of the app (run with `shiny::runApp("app.R")`).
- `R/` — pure, testable modeling functions (growth, vulnerability,
  simulation, summaries, yield curves) exported by the package.
- `tests/` — `testthat` unit tests for the modeling functions.

For dependency metadata, see `DESCRIPTION`.
