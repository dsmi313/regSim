# Sourced automatically by testthat before any test files.
#
# When regSim is installed and loaded (R CMD check on GitHub, or
# devtools::test()), its functions are already on the search path and there is
# nothing to do. When tests are run from a bare clone without the package
# installed, fall back to sourcing the R/ files directly.
if (!requireNamespace("regSim", quietly = TRUE)) {
  this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile),
                        error = function(e) NULL)
  if (!is.null(this_file)) {
    pkg_root <- normalizePath(file.path(dirname(this_file), "..", ".."),
                              mustWork = FALSE)
  } else {
    pkg_root <- getwd()   # fallback for devtools::test() run from package root
  }

  source(file.path(pkg_root, "R", "species_presets.R"))
  source(file.path(pkg_root, "R", "growth_presets.R"))
  source(file.path(pkg_root, "R", "length_bins.R"))
  source(file.path(pkg_root, "R", "model_core.R"))
  source(file.path(pkg_root, "R", "summarize.R"))
  source(file.path(pkg_root, "R", "yield_curve.R"))
  source(file.path(pkg_root, "R", "validation_checks.R"))
}
