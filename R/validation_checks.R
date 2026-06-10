#' Sanity-check simulation results and parameter values
#'
#' Flags biologically implausible or numerically suspect outcomes: spawning
#' potential ratio below the 0.30 overfishing threshold or above 1.0,
#' exploitation rates above 0.80, and recruitment CV above 1.5.
#'
#' @param sim_df A data.frame with at least a numeric \code{SPR} column (the
#'   \code{sim_df} element returned by \code{\link{run_population_simulation}}).
#' @param params Optional named list of parameter values to check; recognised
#'   keys are \code{U} (exploitation rate) and \code{rec_cv}.
#'
#' @return A list with \code{pass} (logical; \code{TRUE} when no warnings were
#'   raised) and \code{warnings} (a character vector of warning messages).
#' @export
run_model_checks <- function(sim_df, params = list()) {
  warnings <- character(0)

  mean_spr <- mean(sim_df$SPR, na.rm = TRUE)

  if (!is.na(mean_spr) && mean_spr < 0.3) {
    warnings <- c(warnings, sprintf(
      "SPR = %.2f is below the 0.30 overfishing threshold. Population may be experiencing recruitment overfishing. Consider reducing exploitation or implementing protective regulations.",
      mean_spr
    ))
  }

  if (!is.na(mean_spr) && mean_spr > 1.0) {
    warnings <- c(warnings, sprintf(
      "SPR = %.2f exceeds 1.0, which likely indicates a model configuration error.",
      mean_spr
    ))
  }

  if (!is.null(params$U) && !is.na(params$U) && params$U > 0.8) {
    warnings <- c(warnings,
      "Exploitation rate (U) > 0.80 is unrealistically high for most managed fisheries.")
  }

  if (!is.null(params$rec_cv) && !is.na(params$rec_cv) && params$rec_cv > 1.5) {
    warnings <- c(warnings,
      "Recruitment CV > 1.5 may produce numerically unstable simulations.")
  }

  list(pass = length(warnings) == 0L, warnings = warnings)
}
