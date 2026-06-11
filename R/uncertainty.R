#' Map an uncertainty level label to a coefficient of variation
#'
#' @param level Character: one of \code{"Off"}, \code{"Low"},
#'   \code{"Medium"}, or \code{"High"}.
#' @return Numeric CV: 0 for Off, 0.10 for Low, 0.20 for Medium, 0.30 for High.
#' @export
get_uncertainty_cv <- function(level) {
  switch(level,
    "Off"    = 0,
    "Low"    = 0.10,
    "Medium" = 0.20,
    "High"   = 0.30,
    stop(paste("Unknown uncertainty level:", level), call. = FALSE)
  )
}


#' Sample mortality parameters with CV-based uncertainty
#'
#' Draws \code{n} parameter sets by adding CV-based noise around the
#' point estimates. \code{nat_mort} is sampled from a lognormal distribution
#' (ensuring positive values). \code{U} and \code{DisMort} are sampled from
#' a beta distribution (ensuring values stay in [0, 1]).
#'
#' @param nat_mort Natural mortality point estimate (positive numeric).
#' @param U Exploitation rate point estimate (in [0, 1]).
#' @param DisMort Discard mortality rate point estimate (in [0, 1]).
#' @param cv Coefficient of variation (non-negative numeric). When \code{0},
#'   all columns equal the point estimates exactly.
#' @param n Number of parameter sets to draw.
#'
#' @return A data.frame with \code{n} rows and columns
#'   \code{nat_mort}, \code{U}, \code{DisMort}.
#' @importFrom stats rlnorm rbeta rnorm
#' @export
sample_mortality_parameters <- function(nat_mort, U, DisMort, cv, n) {
  if (cv <= 0) {
    return(data.frame(
      nat_mort = rep(nat_mort, n),
      U        = rep(U,        n),
      DisMort  = rep(DisMort,  n)
    ))
  }

  sigma_m     <- sqrt(log(cv^2 + 1))
  nm_samples  <- rlnorm(n, meanlog = log(nat_mort) - sigma_m^2 / 2, sdlog = sigma_m)

  data.frame(
    nat_mort = nm_samples,
    U        = .sample_bounded(U,       cv, n),
    DisMort  = .sample_bounded(DisMort, cv, n)
  )
}

# Beta sampling for [0,1]-bounded parameters; clips to normal as fallback
# when the requested CV exceeds what the beta can represent at that mean.
.sample_bounded <- function(mu, cv, n) {
  if (mu <= 0) return(rep(0, n))
  if (mu >= 1) return(rep(1, n))
  phi <- (1 - mu) / (cv^2 * mu) - 1
  if (phi > 0) {
    return(rbeta(n, mu * phi, (1 - mu) * phi))
  }
  pmax(0, pmin(1, rnorm(n, mu, cv * mu)))
}


#' Run the population simulation across sampled parameter sets
#'
#' Draws \code{nsim} parameter sets via
#' \code{\link{sample_mortality_parameters}} and runs one simulation per set
#' (\code{nsim_inner = 1} per draw). Returns the combined per-draw outcomes
#' as a data.frame suitable for \code{\link{summarize_uncertainty_results}}.
#'
#' @param nat_mort,U,DisMort Point estimates for natural mortality, exploitation
#'   rate, and discard mortality.
#' @param cv Coefficient of variation passed to
#'   \code{\link{sample_mortality_parameters}}.
#' @param nsim Number of parameter sets to draw (one simulation per set).
#' @param bin_midpoints,length_bins Length-bin midpoints and edges (mm).
#' @param Growth_matrix,recruit_dist Outputs of
#'   \code{\link{make_growth_matrix}}.
#' @param Vulcap_bins,Vulharv_bins,trophyvul_bins,Fec_bins,Wt_bins
#'   Per-bin arrays from \code{\link{make_vulnerability_curves}}.
#' @param M_bins Per-bin instantaneous natural mortality array from
#'   \code{\link{make_vulnerability_curves}}. Used to rescale survival for
#'   each sampled \code{nat_mort} value.
#' @param Amax Maximum age class (years).
#' @param Ymax Years to simulate.
#' @param Ro,rec_cv Unfished recruitment and recruitment CV.
#' @param enable_ddr,steepness,enable_depensation Passed through to
#'   \code{\link{run_population_simulation}}.
#' @param progress_fn Optional \code{function(k, n)} for progress reporting.
#'
#' @return A data.frame with \code{nsim} rows and columns \code{YPR},
#'   \code{SPR}, \code{Prop}, \code{MeanLengthHarvested}, \code{nat_mort},
#'   \code{U}, \code{DisMort}.
#' @export
run_uncertainty_simulation <- function(nat_mort, U, DisMort, cv, nsim,
                                       bin_midpoints, length_bins,
                                       Growth_matrix, recruit_dist,
                                       Vulcap_bins, Vulharv_bins,
                                       trophyvul_bins, Fec_bins, Wt_bins,
                                       M_bins,
                                       Amax, Ymax, Ro, rec_cv,
                                       enable_ddr         = FALSE,
                                       steepness          = 0.7,
                                       enable_depensation = FALSE,
                                       progress_fn        = NULL) {
  params  <- sample_mortality_parameters(nat_mort, U, DisMort, cv, nsim)
  results <- vector("list", nsim)

  for (k in seq_len(nsim)) {
    if (!is.null(progress_fn)) progress_fn(k, nsim)

    p        <- params[k, ]
    # M_bins = nat_mort * size-class multiplier; rescale proportionally
    S_bins_k <- exp(-M_bins * (p$nat_mort / nat_mort))

    sim_k <- run_population_simulation(
      bin_midpoints  = bin_midpoints,   length_bins   = length_bins,
      Growth_matrix  = Growth_matrix,   recruit_dist  = recruit_dist,
      Vulcap_bins    = Vulcap_bins,     Vulharv_bins  = Vulharv_bins,
      trophyvul_bins = trophyvul_bins,  Fec_bins      = Fec_bins,
      Wt_bins        = Wt_bins,         S_bins        = S_bins_k,
      Amax           = Amax,            Ymax          = Ymax,
      Ro             = Ro,              rec_cv        = rec_cv,
      U              = p$U,             DisMort       = p$DisMort,
      nsim           = 1,
      enable_ddr         = enable_ddr,
      steepness          = steepness,
      enable_depensation = enable_depensation,
      collect_full_output = FALSE
    )

    df_k       <- sim_k$sim_df
    results[[k]] <- data.frame(
      YPR                 = df_k$YPR,
      SPR                 = df_k$SPR,
      Prop                = df_k$Prop,
      MeanLengthHarvested = df_k$MeanLengthHarvested,
      nat_mort            = p$nat_mort,
      U                   = p$U,
      DisMort             = p$DisMort
    )
  }

  do.call(rbind, results)
}


#' Summarise uncertainty simulation results
#'
#' Computes the median, 2.5th, and 97.5th percentiles for each key metric
#' across the draws from \code{\link{run_uncertainty_simulation}}.
#'
#' @param results_df Data.frame returned by
#'   \code{\link{run_uncertainty_simulation}}.
#'
#' @return A data.frame with one row per metric and columns
#'   \code{metric}, \code{median}, \code{lower95}, \code{upper95}.
#' @importFrom stats median quantile
#' @export
summarize_uncertainty_results <- function(results_df) {
  metrics <- c("YPR", "SPR", "Prop", "MeanLengthHarvested")
  rows <- lapply(metrics, function(m) {
    x <- results_df[[m]]
    data.frame(
      metric  = m,
      median  = median(x,              na.rm = TRUE),
      lower95 = quantile(x, 0.025,     na.rm = TRUE),
      upper95 = quantile(x, 0.975,     na.rm = TRUE),
      row.names = NULL
    )
  })
  do.call(rbind, rows)
}
