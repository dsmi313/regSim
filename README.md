# regSim

An interactive Shiny app for fisheries regulation analysis: yield per recruit,
spawning potential ratio, and MSY-type yield curves.

## Why this matters

Managers often need to evaluate how changes in length limits, slot limits,
exploitation, discard mortality, and growth affect yield and spawning potential
before implementing a regulation. Field data can't answer those questions
quickly — the lag between a regulation change and a measurable population
response can be years or decades. regSim gives a transparent simulation
interface for exploring those tradeoffs without writing code.

**Regulation scenarios you can test**

- **Minimum-length limit** — set the size at which fish become harvestable
- **Protective slot limit** — protect fish within a size range (e.g. 12–16 in),
  harvest allowed above and below
- **Traditional slot limit** — harvest restricted to fish *within* the slot,
  protecting large broodstock
- **Maximum-length limit** — cap harvest size to protect large, highly fecund
  fish

**Exploitation and mortality controls**

- **Exploitation rate (U)** — fraction of vulnerable fish harvested each year
- **Discard mortality** — mortality of released fish; important for
  catch-and-release fisheries and regulations that require release of certain
  size classes

**Growth and biology**

- **Species presets** (white crappie, black crappie, walleye, largemouth bass,
  smallmouth bass, channel catfish, blue catfish) with slow / moderate / fast
  growth scenarios
- **Growth CV** — coefficient of variation in length-at-age; higher values
  produce more realistic, less modal length-frequency distributions

**Outputs and metrics**

- **YPR (Yield Per Recruit)** — average harvest weight per recruit across
  stochastic simulations
- **SPR (Spawning Potential Ratio)** — fished spawning biomass as a fraction
  of unfished. Values below 0.30 are often used as a warning threshold, though
  appropriate reference points are species- and system-specific.
- **MSY-type yield curve** — exploitation sweep that identifies the U
  maximising total yield and the corresponding SPR

## Parameter uncertainty

regSim runs deterministic scenarios by default. Setting **Parameter Uncertainty**
to Low, Medium, or High adds CV-based variation (10 / 20 / 30 %) around natural
mortality, exploitation rate, and release/discard mortality. Each simulation draw
uses an independently sampled parameter set; the violin plots and summary
statistics then reflect the distribution of outcomes across those samples.

This is a sensitivity and robustness layer, not a formal Bayesian stock assessment.

**Growth CV is different.** The *Growth CV* slider controls biological variation in
length-at-age within the population — how smooth or modal the length-frequency
distribution appears. Parameter uncertainty applies to the population-level inputs
(M, U, discard mortality), not to individual growth trajectories.

The underlying model is a stochastic, age-structured, length-bin simulation
with von Bertalanffy growth, Beverton-Holt stock-recruit, optional
density-dependent recruitment, and optional depensation (Allee effects).
Vulnerability curves for capture, harvest, and trophy-size fish are built
from the regulation inputs. All model functions are exported from the package,
so batch runs and custom analyses can be scripted directly with
`run_population_simulation()`, `run_yield_curve()`, and friends.

## Install and run (recommended)

Install directly from GitHub as an R package and launch the app:

```r
install.packages("remotes")
remotes::install_github("dsmi313/regSim")
regSim::run_app()
```

That's it — no need to clone the repository or source any files manually. The
Shiny app and all modeling functions ship inside the package. Dependencies
(`shiny`, `dplyr`, `tidyr`, `ggplot2`, `plotly`) are installed automatically.

## Scripted use

regSim is a real R package, not just a Shiny app — every modeling function is
exported, so you can build inputs and run the model directly from scripts:

```r
library(regSim)

# Length-bin grid spanning 0 to 1.2 * Linf in 10 mm steps
bins <- make_length_bins(Linf = 450, bin_width = 10)

# von Bertalanffy growth transition matrix + recruit size distribution
growth <- make_growth_matrix(
  Linf          = 450,
  vbk           = 0.30,
  t0            = -0.5,
  bin_midpoints = bins$bin_midpoints,
  length_bins   = bins$length_bins,
  growth_cv     = 0.10
)

str(growth)   # $Growth_matrix (row-stochastic) and $recruit_dist (sums to 1)
```

From there, `make_vulnerability_curves()` turns regulation settings into
selectivity ogives, `run_population_simulation()` runs the stochastic
age-structured simulation, and `run_yield_curve()` sweeps exploitation to
locate the MSY-type peak — all without launching the app.

## Development

Contributors working from a local clone can run the app straight from the root
`app.R` script (kept as a development backup):

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

| Path | Contents |
|---|---|
| `inst/shiny/app.R` | Packaged Shiny app, launched by `regSim::run_app()` |
| `app.R` | Root development copy, run with `shiny::runApp("app.R")` |
| `R/` | Exported modeling functions for growth, vulnerability, simulation, summaries, and yield curves |
| `tests/` | `testthat` unit tests, run by GitHub Actions |

For dependency metadata, see `DESCRIPTION`.

## Status

This package is under active development. Model outputs are intended for
scenario exploration and teaching, not as a substitute for stock-specific
assessment or formal management advice.
