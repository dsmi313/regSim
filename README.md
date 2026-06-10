# regSim
A YPR SPR app used to support management decisions in fisheries

## Running the app

This repo includes a standalone Shiny application in `app.R`.

Requirements:
- R installed
- R packages: `shiny`, `dplyr`, `tidyr`, `ggplot2`, `plotly`

For dependency metadata, see `DESCRIPTION`.

To install missing R packages first:

```bash
Rscript install_packages.R
```

From the repository root:

```bash
chmod +x run.sh
./run.sh
```

If `Rscript` is not available, install R first and then run:

```bash
Rscript -e "shiny::runApp('app.R', host = '0.0.0.0', port = 3838, launch.browser = TRUE)"
```

