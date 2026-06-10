#' Summarise simulation output into a time-series data frame
#'
#' Collapses the per-year, per-replicate matrices from
#' \code{\link{run_population_simulation}} (run with
#' \code{collect_full_output = TRUE}) into yearly means, standard deviations, and
#' 95\% prediction intervals for YPR, SPR, proportion memorable, and spawning
#' stock biomass.
#'
#' @param sim_out The list returned by \code{\link{run_population_simulation}}
#'   with full output (must contain \code{all_YPR}, \code{all_SPR},
#'   \code{all_Prop}, \code{all_SSB}, and \code{burnin_years}).
#' @param Ymax Number of years simulated.
#'
#' @return A data.frame with one row per year, containing mean/sd/lower/upper
#'   columns for each metric plus \code{burnin_years}.
#' @importFrom stats sd
#' @export
summarize_timeseries <- function(sim_out, Ymax) {
  ts_data <- data.frame(
    Year      = 1:Ymax,
    YPR_mean  = rowMeans(sim_out$all_YPR,  na.rm = TRUE),
    YPR_sd    = apply(sim_out$all_YPR,  1, sd, na.rm = TRUE),
    SPR_mean  = rowMeans(sim_out$all_SPR,  na.rm = TRUE),
    SPR_sd    = apply(sim_out$all_SPR,  1, sd, na.rm = TRUE),
    Prop_mean = rowMeans(sim_out$all_Prop, na.rm = TRUE),
    Prop_sd   = apply(sim_out$all_Prop, 1, sd, na.rm = TRUE),
    SSB_mean  = rowMeans(sim_out$all_SSB,  na.rm = TRUE),
    SSB_sd    = apply(sim_out$all_SSB,  1, sd, na.rm = TRUE)
  )
  ts_data$burnin_years <- sim_out$burnin_years
  ts_data$YPR_lower  <- pmax(0, ts_data$YPR_mean  - 1.96 * ts_data$YPR_sd)
  ts_data$YPR_upper  <- ts_data$YPR_mean  + 1.96 * ts_data$YPR_sd
  ts_data$SPR_lower  <- pmax(0, ts_data$SPR_mean  - 1.96 * ts_data$SPR_sd)
  ts_data$SPR_upper  <- ts_data$SPR_mean  + 1.96 * ts_data$SPR_sd
  ts_data$Prop_lower <- pmax(0, ts_data$Prop_mean - 1.96 * ts_data$Prop_sd)
  ts_data$Prop_upper <- pmin(1, ts_data$Prop_mean + 1.96 * ts_data$Prop_sd)
  ts_data$SSB_lower  <- pmax(0, ts_data$SSB_mean  - 1.96 * ts_data$SSB_sd)
  ts_data$SSB_upper  <- ts_data$SSB_mean  + 1.96 * ts_data$SSB_sd
  ts_data
}


#' Summarise equilibrium length-frequency distribution
#'
#' Collapses the per-replicate equilibrium abundance-at-length matrix into a
#' length-frequency data frame with mean, median, SD, interquartile range, and a
#' 95\% prediction interval, joined to the selectivity curves for plotting.
#'
#' @param sim_out The list returned by \code{\link{run_population_simulation}}
#'   with full output (must contain \code{all_Abundance}).
#' @param bin_midpoints Numeric vector of length-bin midpoints (mm).
#' @param vc The selectivity list from \code{\link{make_vulnerability_curves}}
#'   (uses \code{Wt_bins}, \code{Vulcap_bins}, \code{Vulharv_bins},
#'   \code{trophyvul_bins}).
#'
#' @return A data.frame with one row per length bin.
#' @importFrom stats sd median quantile
#' @export
summarize_length_data <- function(sim_out, bin_midpoints, vc) {
  length_data <- data.frame(
    Length           = bin_midpoints,
    Weight           = vc$Wt_bins,
    Abundance_mean   = rowMeans(sim_out$all_Abundance, na.rm = TRUE),
    Abundance_median = apply(sim_out$all_Abundance, 1, median,   na.rm = TRUE),
    Abundance_sd     = apply(sim_out$all_Abundance, 1, sd,       na.rm = TRUE),
    Abundance_q25    = apply(sim_out$all_Abundance, 1, quantile,
                             probs = 0.25, na.rm = TRUE),
    Abundance_q75    = apply(sim_out$all_Abundance, 1, quantile,
                             probs = 0.75, na.rm = TRUE),
    VulCapture       = vc$Vulcap_bins,
    VulHarvest       = vc$Vulharv_bins,
    VulTrophy        = vc$trophyvul_bins
  )
  length_data$Abundance_lower <- pmax(0,
    length_data$Abundance_mean - 1.96 * length_data$Abundance_sd)
  length_data$Abundance_upper <-
    length_data$Abundance_mean + 1.96 * length_data$Abundance_sd
  length_data
}


#' Summarise equilibrium age distribution
#'
#' Collapses the per-replicate equilibrium abundance-at-age matrix into an age
#' distribution data frame with mean, median, and a 95\% prediction interval.
#'
#' @param sim_out The list returned by \code{\link{run_population_simulation}}
#'   with full output (must contain \code{all_AgeAbundance}).
#' @param Amax Maximum age class (years).
#'
#' @return A data.frame with one row per age class.
#' @importFrom stats sd median
#' @export
summarize_age_data <- function(sim_out, Amax) {
  age_data <- data.frame(
    Age              = 1:Amax,
    Abundance_mean   = rowMeans(sim_out$all_AgeAbundance, na.rm = TRUE),
    Abundance_median = apply(sim_out$all_AgeAbundance, 1, median, na.rm = TRUE),
    Abundance_sd     = apply(sim_out$all_AgeAbundance, 1, sd,     na.rm = TRUE)
  )
  age_data$Abundance_lower <- pmax(0,
    age_data$Abundance_median - 1.96 * age_data$Abundance_sd)
  age_data$Abundance_upper <- age_data$Abundance_median + 1.96 * age_data$Abundance_sd
  age_data
}
