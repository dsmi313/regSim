# Sourced automatically by testthat before any test files.
# Locate the package root by walking up from this file's location.
this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile), error = function(e) NULL)
if (!is.null(this_file)) {
  pkg_root <- normalizePath(file.path(dirname(this_file), "..", ".."), mustWork = FALSE)
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
