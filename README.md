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
  of unfished; values below 0.30 indicate recruitment overfishing risk
- **MSY-type yield curve** — exploitation sweep that identifies the U
  maximising total yield and the corresponding SPR

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
| `app.R` | Root development copy (run with `shiny::runApp("app.R")`) |
| `R/` | Exported modeling functions: growth, vulnerability, simulation, summaries, yield curves |
| `tests/` | `testthat` unit tests, run automatically by GitHub Actions on every PR |

For dependency metadata, see `DESCRIPTION`.
