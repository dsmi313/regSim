#' Build the length-bin growth transition matrix and recruit distribution
#'
#' Constructs the probabilistic length-bin transition matrix and the age-1
#' recruit size distribution from von Bertalanffy growth parameters and a
#' coefficient of variation in length-at-age.
#'
#' @param Linf Asymptotic length (mm), von Bertalanffy \eqn{L_\infty}.
#' @param vbk Brody growth coefficient, von Bertalanffy \eqn{K}.
#' @param t0 Theoretical age at length zero, von Bertalanffy \eqn{t_0}.
#' @param bin_midpoints Numeric vector of length-bin midpoints (mm).
#' @param length_bins Numeric vector of length-bin edges (mm); one longer than
#'   \code{bin_midpoints}.
#' @param growth_cv Coefficient of variation in length-at-age. \code{0} gives a
#'   deterministic matrix (each bin maps to a single destination bin).
#'
#' @return A list with components \code{Growth_matrix} (a row-stochastic
#'   \code{L x L} matrix) and \code{recruit_dist} (a length-\code{L} probability
#'   vector summing to 1).
#' @importFrom stats pnorm
#' @export
make_growth_matrix <- function(Linf, vbk, t0, bin_midpoints, length_bins, growth_cv) {
  L_bins   <- length(bin_midpoints)
  bin_width <- length_bins[2] - length_bins[1]

  Growth_matrix <- matrix(0, nrow = L_bins, ncol = L_bins)
  for (i in seq_len(L_bins)) {
    current_length   <- bin_midpoints[i]
    growth_increment <- max(0.1, (Linf - current_length) * (1 - exp(-vbk)))
    expected_length  <- current_length + growth_increment

    if (growth_cv == 0) {
      next_bin <- which.min(abs(bin_midpoints - expected_length))
      Growth_matrix[i, next_bin] <- 1
      next
    }

    growth_sd <- max(1, growth_increment * growth_cv, bin_width * 0.15)
    if (current_length >= Linf * 0.99) {
      growth_increment <- 0.1
      growth_sd        <- max(1, bin_width * 0.15)
      expected_length  <- current_length + growth_increment
    }

    for (j in seq_len(L_bins)) {
      Growth_matrix[i, j] <- pnorm(length_bins[j + 1], expected_length, growth_sd) -
                              pnorm(length_bins[j],     expected_length, growth_sd)
    }
    row_sum <- sum(Growth_matrix[i, ])
    if (row_sum > 0) {
      Growth_matrix[i, ] <- Growth_matrix[i, ] / row_sum
    } else {
      Growth_matrix[i, i] <- 1.0
    }
  }

  age1_mean_length <- Linf * (1 - exp(-vbk * (1 - t0)))
  recruit_dist     <- rep(0, L_bins)

  if (growth_cv == 0) {
    recruit_dist[which.min(abs(bin_midpoints - age1_mean_length))] <- 1.0
  } else {
    age1_sd_length <- max(0.5, age1_mean_length * growth_cv)
    for (j in seq_len(L_bins)) {
      recruit_dist[j] <- max(0, pnorm(length_bins[j + 1], age1_mean_length, age1_sd_length) -
                                 pnorm(length_bins[j],     age1_mean_length, age1_sd_length))
    }
    if (sum(recruit_dist) > 0) {
      recruit_dist <- recruit_dist / sum(recruit_dist)
    } else {
      recruit_dist[which.min(abs(bin_midpoints - age1_mean_length))] <- 1.0
    }
  }

  list(Growth_matrix = Growth_matrix, recruit_dist = recruit_dist)
}


#' Compute size-based selectivity, fecundity, and natural-mortality arrays
#'
#' Computes all per-length-bin quantities that do not depend on the exploitation
#' rate \code{U}: capture and harvest vulnerability, trophy vulnerability,
#' weight, fecundity, the maturity ogive, and stage-structured natural mortality
#' and survival. Supports minimum-length, traditional/protective slot, and
#' maximum-length regulations.
#'
#' @param bin_midpoints Numeric vector of length-bin midpoints (mm).
#' @param Capsize Length at 50\% capture vulnerability (mm).
#' @param Harvlim Minimum harvest length (mm).
#' @param mat_size Length at maturity (mm).
#' @param memorable_size Memorable/trophy length threshold (mm).
#' @param wl_a,wl_b Weight-length coefficients in \eqn{W = a L^b} (W in kg).
#' @param nat_mort Adult instantaneous natural mortality \eqn{M}.
#' @param fec_exp Fecundity-weight exponent.
#' @param enable_slot Logical; apply a slot limit.
#' @param slot_type \code{"traditional"} (keep within slot) or
#'   \code{"protective"} (protect within slot).
#' @param slot_upper Upper edge of the slot (mm); required when
#'   \code{enable_slot = TRUE}.
#' @param enable_max_limit Logical; apply a maximum-length limit.
#' @param max_harvest_size Maximum harvest length (mm); required when
#'   \code{enable_max_limit = TRUE}.
#'
#' @return A named list of per-bin numeric vectors: \code{Vulcap_bins},
#'   \code{Vulharv_bins}, \code{trophyvul_bins}, \code{Fec_bins}, \code{Wt_bins},
#'   \code{M_bins}, \code{S_bins}, \code{maturity_ogive_bins}.
#' @export
make_vulnerability_curves <- function(bin_midpoints,
                                      Capsize, Harvlim,
                                      mat_size, memorable_size,
                                      wl_a, wl_b, nat_mort, fec_exp,
                                      enable_slot     = FALSE,
                                      slot_type       = "traditional",
                                      slot_upper      = NULL,
                                      enable_max_limit = FALSE,
                                      max_harvest_size = NULL) {
  CapsizeSD  <- Capsize * 0.01
  HarvlimSD  <- Harvlim * 0.01

  Wt_bins            <- (wl_a * bin_midpoints ^ wl_b) / 1000
  Wmat               <- (wl_a * mat_size ^ wl_b) / 1000
  maturity_ogive_bins <- 1 / (1 + exp(-(Wt_bins - Wmat) / (Wmat * 0.1)))
  Fec_bins           <- (Wt_bins ^ fec_exp) * maturity_ogive_bins

  Vulcap_bins <- 1 / (1 + exp(-(bin_midpoints - Capsize) / CapsizeSD))

  if (enable_slot) {
    slope        <- 0.01
    Effective_min        <- max(Harvlim, Capsize)
    Vulharv_above_min    <- 1 / (1 + exp(-(bin_midpoints - Effective_min) / slope))
    Vulharv_below_max    <- 1 / (1 + exp( (bin_midpoints - slot_upper)    / slope))
    if (slot_type == "traditional") {
      Vulharv_bins <- Vulharv_above_min * Vulharv_below_max
    } else {
      Vulharv_bins <- (1 - (Vulharv_above_min * Vulharv_below_max)) * Vulcap_bins
    }
  } else if (enable_max_limit) {
    Vulharv_below_max <- 1 / (1 + exp((bin_midpoints - max_harvest_size) / 0.01))
    Vulharv_bins      <- Vulcap_bins * Vulharv_below_max
  } else {
    Vulharv_bins <- 1 / (1 + exp(-(bin_midpoints - Harvlim) / HarvlimSD))
  }

  trophyvul_bins <- (1 / (1 + exp(-(bin_midpoints - memorable_size) /
                                   (memorable_size * 0.1)))) * Vulcap_bins

  M_adult <- nat_mort
  M_bins  <- rep(M_adult, length(bin_midpoints))
  juv_threshold <- mat_size * 0.5
  M_bins[bin_midpoints <  juv_threshold]                              <- M_adult * 2.0
  M_bins[bin_midpoints >= juv_threshold & bin_midpoints < mat_size]   <- M_adult * 1.5
  S_bins <- exp(-M_bins)

  list(
    Vulcap_bins         = Vulcap_bins,
    Vulharv_bins        = Vulharv_bins,
    trophyvul_bins      = trophyvul_bins,
    Fec_bins            = Fec_bins,
    Wt_bins             = Wt_bins,
    M_bins              = M_bins,
    S_bins              = S_bins,
    maturity_ogive_bins = maturity_ogive_bins
  )
}


#' Age/length-structured stochastic population simulation
#'
#' Runs the core age- and length-structured forward simulation across
#' \code{nsim} stochastic replicates. All pre-computed bin arrays are passed in,
#' so the function is free of Shiny dependencies and fully unit-testable.
#'
#' @param bin_midpoints,length_bins Length-bin midpoints and edges (mm).
#' @param Growth_matrix,recruit_dist Outputs of \code{\link{make_growth_matrix}}.
#' @param Vulcap_bins,Vulharv_bins,trophyvul_bins,Fec_bins,Wt_bins,S_bins
#'   Per-bin arrays from \code{\link{make_vulnerability_curves}}.
#' @param Amax Maximum age class (years).
#' @param Ymax Number of years to simulate.
#' @param Ro Unfished recruitment (number of age-1 fish).
#' @param rec_cv Recruitment coefficient of variation. \code{0} is deterministic.
#' @param U Exploitation rate (proportion of harvestable fish removed annually).
#' @param DisMort Discard (release) mortality rate.
#' @param nsim Number of stochastic replicates.
#' @param enable_ddr Logical; use Beverton-Holt density-dependent recruitment.
#' @param steepness Beverton-Holt steepness \eqn{h} (used when
#'   \code{enable_ddr = TRUE}).
#' @param enable_depensation Logical; apply depensation below 20\% of unfished
#'   spawning stock biomass.
#' @param collect_full_output When \code{TRUE}, also return the per-year,
#'   per-replicate matrices needed for time-series and population-structure
#'   plots; set \code{FALSE} for faster yield-curve sweeps.
#' @param progress_fn Optional \code{function(k, nsim)} called once per replicate
#'   (e.g. to drive a Shiny progress bar).
#'
#' @return A list with \code{sim_df} (data.frame: \code{sim}, \code{YPR},
#'   \code{SPR}, \code{RelEgg}, \code{Prop}, \code{MeanLengthHarvested},
#'   \code{Recruit}) and \code{burnin_years}. \code{SPR} is the deterministic
#'   per-recruit spawning potential ratio (Walters & Martell incidence
#'   function, bounded \eqn{\le 1}); \code{RelEgg} is stochastic stock-level egg
#'   production relative to the unfished equilibrium and may exceed 1 in
#'   favourable recruitment years. When \code{collect_full_output = TRUE}, also
#'   \code{all_YPR}, \code{all_SPR}, \code{all_RelEgg}, \code{all_Prop},
#'   \code{all_EggProd}, \code{all_Abundance}, and \code{all_AgeAbundance}.
#' @importFrom stats rlnorm
#' @export
run_population_simulation <- function(bin_midpoints, length_bins,
                                      Growth_matrix, recruit_dist,
                                      Vulcap_bins, Vulharv_bins, trophyvul_bins,
                                      Fec_bins, Wt_bins, S_bins,
                                      Amax, Ymax,
                                      Ro, rec_cv, U, DisMort, nsim,
                                      enable_ddr        = FALSE,
                                      steepness         = 0.7,
                                      enable_depensation = FALSE,
                                      collect_full_output = TRUE,
                                      progress_fn       = NULL) {
  L_bins <- length(bin_midpoints)

  # --- Fishing mortality derived quantities -----------------------------------
  F_bins            <- Vulharv_bins * U
  Release_mort_bins <- (Vulcap_bins - Vulharv_bins) * U * DisMort
  Survival_bins     <- S_bins * (1 - F_bins) * (1 - Release_mort_bins)

  sigmaR       <- sqrt(log(rec_cv ^ 2 + 1))
  burnin_years <- min(Ymax, Amax + 20)

  # --- Spawning potential ratio (deterministic incidence function) -----------
  # Walters & Martell / Allen & Hightower per-recruit approach: trace one
  # recruit through the fished vs. unfished life history, accumulating egg
  # production (Fec_bins) at each age. Recruitment cancels, so
  # SPR = phi_F / phi_0 is bounded <= 1 by construction (fishing only lowers
  # survival). This depends solely on fixed inputs, so it is computed once.
  pr_unfished <- recruit_dist
  pr_fished   <- recruit_dist
  phi_0 <- sum(pr_unfished * Fec_bins)
  phi_F <- sum(pr_fished   * Fec_bins)
  for (a in 2:Amax) {
    pr_unfished <- as.vector(as.vector(pr_unfished * S_bins)        %*% Growth_matrix)
    pr_fished   <- as.vector(as.vector(pr_fished   * Survival_bins) %*% Growth_matrix)
    phi_0 <- phi_0 + sum(pr_unfished * Fec_bins)
    phi_F <- phi_F + sum(pr_fished   * Fec_bins)
  }
  SPR_value <- phi_F / max(1e-12, phi_0)

  # --- Per-simulation output storage -----------------------------------------
  sim_df <- data.frame(
    sim                = seq_len(nsim),
    YPR                = rep(NA_real_, nsim),
    SPR                = rep(NA_real_, nsim),
    RelEgg             = rep(NA_real_, nsim),
    Prop               = rep(NA_real_, nsim),
    MeanLengthHarvested = rep(NA_real_, nsim),
    Recruit            = rep(NA_real_, nsim)
  )

  if (collect_full_output) {
    all_YPR          <- matrix(NA_real_, Ymax, nsim)
    all_SPR          <- matrix(NA_real_, Ymax, nsim)
    all_RelEgg       <- matrix(NA_real_, Ymax, nsim)
    all_Prop         <- matrix(NA_real_, Ymax, nsim)
    all_EggProd      <- matrix(NA_real_, Ymax, nsim)
    all_Abundance    <- matrix(NA_real_, L_bins, nsim)
    all_AgeAbundance <- matrix(NA_real_, Amax,   nsim)
  }

  for (k in seq_len(nsim)) {
    if (!is.null(progress_fn)) progress_fn(k, nsim)

    # --- Per-replicate storage ------------------------------------------------
    N       <- matrix(0, Ymax, L_bins)
    age_len <- matrix(0, Amax, L_bins)
    Yield   <- rep(NA_real_, Ymax)
    SPRt    <- rep(NA_real_, Ymax)
    RelEgg_t <- rep(NA_real_, Ymax)
    YPR     <- rep(NA_real_, Ymax)
    Prop    <- rep(NA_real_, Ymax)
    eggs_t  <- rep(NA_real_, Ymax)

    # --- Burn-in: build unfished equilibrium (no harvest) --------------------
    # Deterministic (constant Ro) so the unfished egg-production reference
    # converges to Ro * phi_0 exactly, giving a clean denominator for the
    # relative-egg-production metric below.
    age_len[1, ] <- Ro * recruit_dist
    N[1, ]       <- colSums(age_len)
    eggs_burnin  <- rep(NA_real_, burnin_years)
    eggs_burnin[1] <- sum(N[1, ] * Fec_bins)

    for (init_year in 2:burnin_years) {
      age_survive <- age_len * matrix(S_bins, nrow = Amax, ncol = L_bins, byrow = TRUE)
      new_age_len <- matrix(0, Amax, L_bins)
      for (a in 1:(Amax - 1)) {
        new_age_len[a + 1, ] <- as.vector(age_survive[a, ] %*% Growth_matrix)
      }
      new_age_len[1, ] <- new_age_len[1, ] + Ro * recruit_dist
      age_len          <- new_age_len
      N[init_year, ]   <- colSums(age_len)
      eggs_burnin[init_year] <- sum(N[init_year, ] * Fec_bins)
    }

    # --- Unfished egg-production reference (denominator for relative eggs) ----
    burnin_start <- max(1L, burnin_years - 9L)
    SPR_denom    <- mean(eggs_burnin[burnin_start:burnin_years], na.rm = TRUE)
    SSB0         <- SPR_denom

    # --- Stochastic recruitment capacity for the fished period ---------------
    if (isTRUE(enable_ddr)) {
      Rcapacity <- rep(NA_real_, Ymax)
    } else if (rec_cv == 0) {
      Rcapacity <- rep(Ro, Ymax)
    } else {
      Rcapacity <- Ro * rlnorm(Ymax, 0, sd = sigmaR)
    }

    # --- Annotate burn-in years (U = 0, no harvest) -------------------------
    for (yr in seq_len(burnin_years)) {
      Yield[yr]   <- 0
      eggs_t[yr]  <- sum(N[yr, ] * Fec_bins)
      RelEgg_t[yr] <- eggs_t[yr] / SPR_denom
      SPRt[yr]    <- 1.0   # unfished during burn-in (U = 0)
      YPR[yr]     <- 0
      Prop[yr]    <- sum(trophyvul_bins * N[yr, ]) / max(1, sum(N[yr, ]))
    }

    # --- Fished simulation years ---------------------------------------------
    start_year <- min(burnin_years + 1L, Ymax)
    for (i in start_year:Ymax) {
      if (isTRUE(enable_ddr)) {
        eggs_prev <- max(0, sum(N[i - 1, ] * Fec_bins))
        R_BH  <- (4 * steepness * Ro * eggs_prev) /
                 (SSB0 * (1 - steepness) + (5 * steepness - 1) * eggs_prev)
        R_BH  <- max(1, R_BH)
        if (isTRUE(enable_depensation) && eggs_prev < 0.2 * SSB0) {
          R_BH <- R_BH * (eggs_prev / (0.2 * SSB0)) ^ 2
        }
        Rcapacity[i] <- if (rec_cv == 0) max(1, R_BH) else
          max(1, R_BH * rlnorm(1, 0, sd = sigmaR))
      }

      age_survive <- age_len *
        matrix(Survival_bins, nrow = Amax, ncol = L_bins, byrow = TRUE)
      new_age_len <- matrix(0, Amax, L_bins)
      for (a in 1:(Amax - 1)) {
        new_age_len[a + 1, ] <- as.vector(age_survive[a, ] %*% Growth_matrix)
      }
      new_age_len[1, ] <- new_age_len[1, ] + Rcapacity[i] * recruit_dist
      age_len          <- new_age_len
      N[i, ]           <- colSums(age_len)
      Yield[i]         <- sum(Wt_bins * Vulharv_bins * N[i, ]) * U
      eggs_t[i]        <- sum(N[i, ] * Fec_bins)
      YPR[i]           <- Yield[i] / max(1, Rcapacity[i])

      # SPR is the deterministic per-recruit value (constant across years);
      # RelEgg is the stochastic stock-level egg production relative to the
      # unfished equilibrium and can exceed 1 in favourable recruitment years.
      SPRt[i]          <- SPR_value
      RelEgg_t[i]      <- eggs_t[i] / SPR_denom
      Prop[i]          <- sum(trophyvul_bins * N[i, ]) / max(1, sum(N[i, ]))
    }

    # --- Summarise last 50 fished years into sim_df row ----------------------
    last_50_start <- max(start_year, Ymax - 49L)
    idx           <- last_50_start:Ymax
    sim_df$SPR[k]     <- SPR_value
    sim_df$RelEgg[k]  <- mean(RelEgg_t[idx], na.rm = TRUE)
    sim_df$YPR[k]     <- mean(YPR[idx],      na.rm = TRUE)
    sim_df$Prop[k]    <- mean(Prop[idx],     na.rm = TRUE)
    sim_df$Recruit[k] <- mean(Rcapacity[idx], na.rm = TRUE)

    harvest_lengths <- vapply(idx, function(yr) {
      hb <- N[yr, ] * Vulharv_bins * U
      th <- sum(hb)
      if (th > 0) sum(hb * bin_midpoints) / th else NA_real_
    }, numeric(1))
    sim_df$MeanLengthHarvested[k] <- mean(harvest_lengths, na.rm = TRUE)

    if (collect_full_output) {
      all_YPR[, k]          <- YPR
      all_SPR[, k]          <- SPRt
      all_RelEgg[, k]       <- RelEgg_t
      all_Prop[, k]         <- Prop
      all_EggProd[, k]      <- eggs_t
      all_Abundance[, k]    <- N[Ymax, ]
      all_AgeAbundance[, k] <- rowSums(age_len)
    }
  }

  out <- list(sim_df = sim_df, burnin_years = burnin_years)
  if (collect_full_output) {
    out$all_YPR          <- all_YPR
    out$all_SPR          <- all_SPR
    out$all_RelEgg       <- all_RelEgg
    out$all_Prop         <- all_Prop
    out$all_EggProd      <- all_EggProd
    out$all_Abundance    <- all_Abundance
    out$all_AgeAbundance <- all_AgeAbundance
  }
  out
}
