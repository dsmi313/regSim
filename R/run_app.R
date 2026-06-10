#' Launch the regSim Shiny application
#'
#' Starts the interactive YPR/SPR Shiny app that ships with the package.
#'
#' @param ... Additional arguments passed to [shiny::runApp()] (for example
#'   `host`, `port`, or `launch.browser`).
#'
#' @return Called for its side effect of launching the Shiny app. Does not
#'   return until the app is stopped.
#'
#' @examples
#' \dontrun{
#' regSim::run_app()
#' }
#'
#' @export
run_app <- function(...) {
  app_dir <- system.file("shiny", "app.R", package = "regSim")
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the packaged Shiny app. ",
      "Try reinstalling regSim with remotes::install_github(\"dsmi313/regSim\").",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, ...)
}
