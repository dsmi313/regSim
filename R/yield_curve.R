#' Generate an equilibrium yield curve across exploitation rates
#'
#' Sweeps a set of exploitation rates, running
#' \code{\link{run_population_simulation}} at each and aggregating the replicate
#' results into per-rate means and standard deviations for YPR, SPR, proportion
#' memorable, equilibrium recruitment, and total yield.
#'
#' @param bin_midpoints,length_bins Length-bin midpoints and edges (mm).
#' @param Growth_matrix,recruit_dist Outputs of \code{\link{make_growth_matrix}}.
#' @param Vulcap_bins,Vulharv_bins,trophyvul_bins,Fec_bins,Wt_bins,S_bins
#'   Per-bin arrays from \code{\link{make_vulnerability_curves}}.
#' @param Amax Maximum age class (years).
#' @param Ymax Number of years to simulate per rate.
#' @param Ro Unfished recruitment (number of age-1 fish).
#' @param rec_cv Recruitment coefficient of variation.
#' @param DisMort Discard (release) mortality rate.
#' @param nsim Number of stochastic replicates per rate.
#' @param U_values Numeric vector of exploitation rates to evaluate. Defaults to
#'   \code{seq(0, 1, by = 0.1)}.
#' @param enable_ddr,steepness,enable_depensation Recruitment options passed
#'   through to \code{\link{run_population_simulation}}.
#' @param progress_fn Optional \code{function(u_idx, n_points)} called once per
#'   exploitation rate (e.g. to drive a Shiny progress bar).
#'
#' @return A data.frame with one row per exploitation rate and columns
#'   \code{U}, plus mean/sd (and replicate count) for YPR, SPR, proportion
#'   memorable, recruitment, and total yield.
#' @importFrom stats sd
#' @export
run_yield_curve <- function(bin_midpoints, length_bins,
                            Growth_matrix, recruit_dist,
                            Vulcap_bins, Vulharv_bins, trophyvul_bins,
                            Fec_bins, Wt_bins, S_bins,
                            Amax, Ymax,
                            Ro, rec_cv, DisMort, nsim,
                            U_values          = seq(0, 1, by = 0.1),
                            enable_ddr        = FALSE,
                            steepness         = 0.7,
                            enable_depensation = FALSE,
                            progress_fn       = NULL) {
  n_points <- length(U_values)

  curve_results <- data.frame(
    U               = U_values,
    YPR_mean        = numeric(n_points), YPR_sd        = numeric(n_points),
    YPR_n           = integer(n_points),
    SPR_mean        = numeric(n_points), SPR_sd        = numeric(n_points),
    SPR_n           = integer(n_points),
    Prop_mean       = numeric(n_points), Prop_sd       = numeric(n_points),
    Prop_n          = integer(n_points),
    Recruit_mean    = numeric(n_points), Recruit_sd    = numeric(n_points),
    TotalYield_mean = numeric(n_points), TotalYield_sd = numeric(n_points)
  )

  for (u_idx in seq_len(n_points)) {
    if (!is.null(progress_fn)) progress_fn(u_idx, n_points)

    sim_out <- run_population_simulation(
      bin_midpoints  = bin_midpoints,    length_bins   = length_bins,
      Growth_matrix  = Growth_matrix,    recruit_dist  = recruit_dist,
      Vulcap_bins    = Vulcap_bins,      Vulharv_bins  = Vulharv_bins,
      trophyvul_bins = trophyvul_bins,   Fec_bins      = Fec_bins,
      Wt_bins        = Wt_bins,          S_bins        = S_bins,
      Amax = Amax, Ymax = Ymax,
      Ro = Ro, rec_cv = rec_cv,
      U = U_values[u_idx], DisMort = DisMort,
      nsim = nsim,
      enable_ddr         = enable_ddr,
      steepness          = steepness,
      enable_depensation = enable_depensation,
      collect_full_output = FALSE
    )

    df               <- sim_out$sim_df
    total_yield_vals <- df$YPR * df$Recruit

    curve_results$YPR_mean[u_idx]        <- mean(df$YPR,        na.rm = TRUE)
    curve_results$YPR_sd[u_idx]          <- sd(df$YPR,          na.rm = TRUE)
    curve_results$YPR_n[u_idx]           <- nsim
    curve_results$SPR_mean[u_idx]        <- mean(df$SPR,        na.rm = TRUE)
    curve_results$SPR_sd[u_idx]          <- sd(df$SPR,          na.rm = TRUE)
    curve_results$SPR_n[u_idx]           <- nsim
    curve_results$Prop_mean[u_idx]       <- mean(df$Prop,       na.rm = TRUE)
    curve_results$Prop_sd[u_idx]         <- sd(df$Prop,         na.rm = TRUE)
    curve_results$Prop_n[u_idx]          <- nsim
    curve_results$Recruit_mean[u_idx]    <- mean(df$Recruit,    na.rm = TRUE)
    curve_results$Recruit_sd[u_idx]      <- sd(df$Recruit,      na.rm = TRUE)
    curve_results$TotalYield_mean[u_idx] <- mean(total_yield_vals, na.rm = TRUE)
    curve_results$TotalYield_sd[u_idx]   <- sd(total_yield_vals,   na.rm = TRUE)
  }
  curve_results
}


#' Locate maximum sustainable yield on a yield curve
#'
#' Finds the exploitation rate that maximises mean total yield on a curve
#' produced by \code{\link{run_yield_curve}}.
#'
#' @param curve_data A data.frame from \code{\link{run_yield_curve}} (must
#'   contain \code{U} and \code{TotalYield_mean}).
#'
#' @return A list with \code{idx} (row index of the maximum), \code{U} (the
#'   exploitation rate at MSY, as a proportion), and \code{total_yield} (the
#'   maximum mean total yield).
#' @export
compute_msy <- function(curve_data) {
  idx <- which.max(curve_data$TotalYield_mean)
  list(
    idx         = idx,
    U           = curve_data$U[idx],
    total_yield = curve_data$TotalYield_mean[idx]
  )
}
