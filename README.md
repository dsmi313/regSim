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
- **Harvest slot limit** — harvest restricted to fish *within* the slot,
  protecting large broodstock
- **Maximum-length limit** — cap harvest size to protect large, highly fecund
  fish

**Exploitation and mortality controls**

- **Exploitation rate (U)** — fraction of vulnerable fish harvested each year
- **Discard mortality** — mortality of released fish; important for
  catch-and-release fisheries and regulations that require release of certain
  size classes

**Growth and biology**

- **Species presets** (white crappie, black crappie, walleye, largemouth bass
  north and south, smallmouth bass, channel catfish, blue catfish) with slow /
  moderate / fast growth scenarios. The largemouth bass growth records are in
  `inst/extdata/lmb_growth_presets.xlsx`
- **Growth CV** — coefficient of variation in length at age (default 0.15),
  held constant across ages. Fish keep their relative size as they age, so
  the spread of lengths grows with age and some fish grow past L∞ (the length
  grid runs to 1.6 × L∞). Higher values produce less modal length-frequency
  distributions

**Outputs and metrics**

- **YPR (Yield Per Recruit)** — total harvest weight (kg) divided by total
  recruitment over the final 50 years of each replicate
- **SPR (Spawning Potential Ratio)** — egg production under fishing as a
  fraction of unfished equilibrium egg production. Values below 0.30 are often
  used as a warning threshold, though appropriate reference points are
  species- and system-specific. Single replicates can exceed 1 during runs of
  strong recruitment.
- **PSD-M** — proportional size distribution of memorable-size fish: the
  proportion of stock-size fish that are memorable size (Gabelhouse 1984
  length categories; Guy et al. 2007)
- **MSY-type yield curve** — exploitation sweep that identifies the U
  maximising total yield and the corresponding SPR

## Parameter uncertainty

Every run is stochastic in recruitment: annual recruits vary lognormally
around unfished recruitment (set by **Recruitment CV**), and results are
summarized across Monte Carlo replicates. Parameter uncertainty is off by
default and has two separate controls, each with Off, Low, Medium, or High
(CV of 10 / 20 / 30 %):

- **Mortality uncertainty** samples natural mortality (M), exploitation rate
  (U), and discard mortality around their input values.
- **Growth uncertainty** samples a single growth-vigor factor that scales L∞
  up and K down together (or the reverse), keeping L∞ × K constant.

Each simulation draw uses its own sampled parameter set, so the violin plots
and summary statistics reflect the spread of outcomes across those samples.
This is a sensitivity and robustness layer, not a formal Bayesian stock
assessment.

**Growth CV is different.** The *Growth CV* input sets individual variation in
length at age within the population, that is, how spread out the
length-frequency distribution is. Growth uncertainty instead reflects
uncertainty in the population's mean growth curve (L∞ and K).

The underlying model is a stochastic, age- and length-structured simulation
with von Bertalanffy mean growth, age-specific growth transition matrices
across 10-mm length bins, and lognormal recruitment that is independent of
stock size by default. Beverton-Holt density-dependent recruitment and
depensation (Allee effects) are optional. Vulnerability curves for capture,
harvest, stock-size, and memorable-size fish are built from the regulation
inputs. All model functions are exported from the package,
so batch runs and custom analyses can be scripted directly with
`run_population_simulation()`, `run_yield_curve()`, and friends.

## Install and run (recommended)

Install directly from GitHub as an R package and launch the app:

```r
install.packages("remotes")
remotes::install_github("dsmi313/regSim", upgrade=F)
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

# Length-bin grid spanning 0 to 1.6 * Linf in 10 mm steps
bins <- make_length_bins(Linf = 450, bin_width = 10)

# Age-specific growth transition matrices + age-1 length distribution
growth <- make_growth_matrix(
  Linf          = 450,
  vbk           = 0.30,
  t0            = -0.5,
  bin_midpoints = bins$bin_midpoints,
  length_bins   = bins$length_bins,
  growth_cv     = 0.15,
  Amax          = 10
)

str(growth)
# $Growth_matrix: L x L x (Amax - 1) array; slice a moves fish from age a to a + 1
# $recruit_dist: age-1 length distribution (sums to 1)
# $mean_length_at_age: von Bertalanffy mean length at ages 1 to Amax
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
