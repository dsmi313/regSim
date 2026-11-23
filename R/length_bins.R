#' Construct the length-bin grid
#'
#' Builds the length-bin edges and midpoints used throughout the model. Bins
#' span 0 to \code{max_mult} (default 1.6) times \eqn{L_\infty} (rounded up to a whole bin) in steps of
#' \code{bin_width}.
#'
#' @param Linf Asymptotic length (mm), von Bertalanffy \eqn{L_\infty}.
#' @param bin_width Length-bin width (mm). Defaults to 10.
#' @param max_mult Upper end of the grid as a multiple of \eqn{L_\infty}.
#'   Defaults to 1.6, wide enough for fish that grow past \eqn{L_\infty}
#'   under the length-at-age growth CV.
#'
#' @return A list with \code{length_bins} (bin edges, mm), \code{bin_midpoints}
#'   (bin midpoints, mm), and \code{L_bins} (the number of bins).
#' @export
make_length_bins <- function(Linf, bin_width = 10, max_mult = 1.6) {
  max_length    <- ceiling(Linf * max_mult)
  length_bins   <- seq(0, max_length, by = bin_width)
  L_bins        <- length(length_bins) - 1
  bin_midpoints <- (length_bins[-1] + length_bins[-(L_bins + 1)]) / 2
  list(length_bins = length_bins, bin_midpoints = bin_midpoints, L_bins = L_bins)
}
