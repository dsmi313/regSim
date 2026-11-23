#' Build the age-specific growth transition matrices and recruit distribution
#'
#' Growth variability is a coefficient of variation in length at age: at every
#' age \eqn{a}, length is approximately Normal with mean
#' \eqn{\mu_a = L_\infty (1 - e^{-K (a - t_0)})} and standard deviation
#' \code{growth_cv} \eqn{\times \mu_a}. Fish keep their relative size from year
#' to year: a fish of length \eqn{L} at age \eqn{a} grows to
#' \eqn{L \mu_{a+1} / \mu_a}, which is equivalent to each fish carrying its own
#' asymptotic length. Absolute spread therefore grows with age and individual
#' fish can exceed the mean \eqn{L_\infty}. Within a length bin fish are treated
#' as uniformly distributed, and each bin's fish are spread over the
#' destination bins their scaled range overlaps (numbers are conserved; fish
#' beyond the last bin edge go to the last bin).
#'
#' @param Linf Asymptotic length (mm), von Bertalanffy \eqn{L_\infty}.
#' @param vbk Brody growth coefficient, von Bertalanffy \eqn{K}.
#' @param t0 Theoretical age at length zero, von Bertalanffy \eqn{t_0}.
#' @param bin_midpoints Numeric vector of length-bin midpoints (mm).
#' @param length_bins Numeric vector of length-bin edges (mm); one longer than
#'   \code{bin_midpoints}.
#' @param growth_cv Coefficient of variation in length at age (default 0.15).
#'   \code{0} gives deterministic growth (each bin maps to a single bin).
#' @param Amax Maximum age class (years). One transition matrix is built for
#'   each age 1 to \code{Amax - 1}.
#'
#' @return A list with \code{Growth_matrix}, an \code{L x L x (Amax - 1)} array
#'   whose slice \code{a} is the row-stochastic transition from age \code{a} to
#'   age \code{a + 1}; \code{recruit_dist}, the age-1 length distribution
#'   (sums to 1); and \code{mean_length_at_age} (\eqn{\mu_1, ..., \mu_{Amax}}).
#' @importFrom stats pnorm
#' @export
make_growth_matrix <- function(Linf, vbk, t0, bin_midpoints, length_bins,
                               growth_cv = 0.15, Amax) {
  if (missing(Amax) || is.null(Amax) || Amax < 2)
    stop("make_growth_matrix(): Amax (maximum age, >= 2) is required.")
  Amax   <- as.integer(Amax)
  L_bins <- length(bin_midpoints)
  lo     <- length_bins[-(L_bins + 1)]
  hi     <- length_bins[-1]

  # Mean length at age, floored at 1 mm so the growth ratios stay finite
  mu <- pmax(1, Linf * (1 - exp(-vbk * (seq_len(Amax) - t0))))

  Growth_matrix <- array(0, dim = c(L_bins, L_bins, Amax - 1L))
  for (a in seq_len(Amax - 1L)) {
    r <- mu[a + 1L] / mu[a]
    G <- matrix(0, L_bins, L_bins)
    for (i in seq_len(L_bins)) {
      if (growth_cv == 0) {
        G[i, which.min(abs(bin_midpoints - bin_midpoints[i] * r))] <- 1
        next
      }
      new_lo <- lo[i] * r
      new_hi <- hi[i] * r
      overlap <- pmax(0, pmin(hi, new_hi) - pmax(lo, new_lo))
      beyond  <- max(0, new_hi - max(hi[L_bins], new_lo))
      overlap[L_bins] <- overlap[L_bins] + beyond
      tot <- sum(overlap)
      if (tot > 0) G[i, ] <- overlap / tot else G[i, L_bins] <- 1
    }
    Growth_matrix[, , a] <- G
  }

  # Age-1 recruits: Normal(mu_1, growth_cv * mu_1), binned
  recruit_dist <- rep(0, L_bins)
  if (growth_cv == 0) {
    recruit_dist[which.min(abs(bin_midpoints - mu[1]))] <- 1.0
  } else {
    recruit_dist <- pmax(0, pnorm(hi, mu[1], growth_cv * mu[1]) -
                            pnorm(lo, mu[1], growth_cv * mu[1]))
    if (sum(recruit_dist) > 0) {
      recruit_dist <- recruit_dist / sum(recruit_dist)
    } else {
      recruit_dist[which.min(abs(bin_midpoints - mu[1]))] <- 1.0
    }
  }

  list(Growth_matrix = Growth_matrix, recruit_dist = recruit_dist,
       mean_length_at_age = mu)
}


#' Compute size-based selectivity, fecundity, and natural-mortality arrays
#'
#' Computes all per-length-bin quantities that do not depend on the exploitation
#' rate \code{U}: capture and harvest vulnerability, trophy vulnerability,
#' weight, fecundity, the maturity ogive, and stage-structured natural mortality
#' and survival. Supports minimum-length, harvest/protective slot, and
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
#' @param slot_type \code{"harvest"} (keep within slot) or
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
                                      slot_type       = "harvest",
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
    # "traditional" is accepted as the pre-0.2.0 name for a harvest slot.
    if (slot_type %in% c("harvest", "traditional")) {
      Vulharv_bins <- Vulharv_above_min * Vulharv_below_max
    } else {
      Vulharv_bins <- (1 - (Vulharv_above_min * Vulharv_below_max)) * Vulcap_bins
    }
  } else if (enable_max_limit) {
    Vulharv_below_max <- 1 / (1 + exp((bin_midpoints - max_harvest_size) / 0.01))
    Vulharv_bins      <- Vulcap_bins * Vulharv_below_max
  } else {
    Vulharv_bins <- Vulcap_bins * (1 / (1 + exp(-(bin_midpoints - Harvlim) / HarvlimSD)))
  }

  trophyvul_bins <- 1 / (1 + exp(-(bin_midpoints - memorable_size) /
                                   (memorable_size * 0.1)))

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
#'   \code{Growth_matrix} is normally the \code{L x L x (Amax - 1)} age-specific
#'   array; a single \code{L x L} matrix applied to every age is also accepted.
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
#'   \code{SPR}, \code{Prop}, \code{MeanLengthHarvested}, \code{Recruit}) and
#'   \code{burnin_years}. \code{SPR} is stock egg production relative to the
#'   unfished equilibrium; values above 1 occur in favourable recruitment years.
#'   When \code{collect_full_output = TRUE}, also \code{all_YPR}, \code{all_SPR},
#'   \code{all_Prop}, \code{all_EggProd}, \code{all_Abundance}, and
#'   \code{all_AgeAbundance}.
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

  # --- Growth: age-specific matrices (array) or one matrix for all ages -------
  if (length(dim(Growth_matrix)) == 3L) {
    if (dim(Growth_matrix)[3] != Amax - 1L)
      stop("Growth_matrix has ", dim(Growth_matrix)[3], " age slices but Amax - 1 = ",
           Amax - 1L, "; build it with make_growth_matrix(..., Amax = Amax).")
    G_list <- lapply(seq_len(Amax - 1L), function(a) Growth_matrix[, , a])
  } else {
    G_list <- NULL
  }
  grow <- function(age_survive) {
    out <- matrix(0, Amax, L_bins)
    if (is.null(G_list)) {
      out[2:Amax, ] <- age_survive[1:(Amax - 1), , drop = FALSE] %*% Growth_matrix
    } else {
      for (a in seq_len(Amax - 1L)) out[a + 1L, ] <- age_survive[a, ] %*% G_list[[a]]
    }
    out
  }

  # --- Pre-compute constant arrays (invariant across all replicates) ----------
  S_mat               <- matrix(S_bins,       nrow = Amax, ncol = L_bins, byrow = TRUE)
  Survival_mat        <- matrix(Survival_bins, nrow = Amax, ncol = L_bins, byrow = TRUE)
  harvest_weight_bins <- Wt_bins * Vulharv_bins * U
  harvest_number_bins <- Vulharv_bins * U

  # --- Pre-compute deterministic burn-in (constant across ALL replicates) ------
  # Burn-in uses only S_mat, constant Ro * recruit_dist, and zero random draws,
  # so the result is identical for every k. Compute once and copy into each
  # replicate instead of re-running burnin_years of matrix algebra nsim times.
  burnin_age_len        <- matrix(0, Amax, L_bins)
  burnin_age_len[1, ]   <- Ro * recruit_dist
  N_burnin              <- matrix(0, burnin_years, L_bins)
  N_burnin[1, ]         <- colSums(burnin_age_len)
  eggs_burnin_ref       <- numeric(burnin_years)
  eggs_burnin_ref[1]    <- sum(N_burnin[1, ] * Fec_bins)
  for (init_year in 2:burnin_years) {
    age_survive                <- burnin_age_len * S_mat
    burnin_age_len             <- grow(age_survive)
    burnin_age_len[1, ]        <- Ro * recruit_dist
    N_burnin[init_year, ]      <- colSums(burnin_age_len)
    eggs_burnin_ref[init_year] <- sum(N_burnin[init_year, ] * Fec_bins)
  }
  burnin_start_ref <- max(1L, burnin_years - 9L)
  SPR_denom_ref    <- mean(eggs_burnin_ref[burnin_start_ref:burnin_years])
  SSB0_ref         <- SPR_denom_ref
  eggs_prev_ref    <- eggs_burnin_ref[burnin_years]

  # --- Per-simulation output storage -----------------------------------------
  sim_df <- data.frame(
    sim                = seq_len(nsim),
    YPR                = rep(NA_real_, nsim),
    SPR                = rep(NA_real_, nsim),
    Prop               = rep(NA_real_, nsim),
    MeanLengthHarvested = rep(NA_real_, nsim),
    Recruit            = rep(NA_real_, nsim)
  )

  if (collect_full_output) {
    all_YPR          <- matrix(NA_real_, Ymax, nsim)
    all_SPR          <- matrix(NA_real_, Ymax, nsim)
    all_Prop         <- matrix(NA_real_, Ymax, nsim)
    all_EggProd      <- matrix(NA_real_, Ymax, nsim)
    all_Abundance    <- matrix(NA_real_, L_bins, nsim)
    all_AgeAbundance <- matrix(NA_real_, Amax,   nsim)
  }

  for (k in seq_len(nsim)) {
    if (!is.null(progress_fn)) progress_fn(k, nsim)

    # ── FAST PATH: collect_full_output = FALSE ─────────────────────────────
    # Avoids allocating N[Ymax, L_bins] and full year-vectors; uses only the
    # current age_len matrix plus scalar accumulators for the final-50-year
    # summary. RNG draw order matches the full path exactly (same seed → same
    # sim_df), so the equivalence test set.seed(x) full == set.seed(x) fast.
    if (!collect_full_output) {
      age_len   <- burnin_age_len   # copy of pre-computed end-of-burnin state
      SPR_denom <- SPR_denom_ref
      SSB0      <- SSB0_ref

      if (isTRUE(enable_ddr)) {
        Rcapacity <- rep(NA_real_, Ymax)
      } else if (rec_cv == 0) {
        Rcapacity <- rep(Ro, Ymax)
      } else {
        Rcapacity <- Ro * rlnorm(Ymax, meanlog = -0.5 * sigmaR^2, sd = sigmaR)
      }

      start_year    <- min(burnin_years + 1L, Ymax)
      last_50_start <- max(start_year, Ymax - 49L)
      n50           <- Ymax - last_50_start + 1L

      SPR_acc <- Yield_acc <- Prop_acc <- Recruit_acc <- 0.0
      harv_len_sum <- 0.0
      harv_len_n   <- 0L
      eggs_prev    <- eggs_prev_ref

      for (i in start_year:Ymax) {
        if (isTRUE(enable_ddr)) {
          eggs_prev <- max(0, eggs_prev)
          R_BH      <- (4 * steepness * Ro * eggs_prev) /
                       (SSB0 * (1 - steepness) + (5 * steepness - 1) * eggs_prev)
          R_BH      <- max(1, R_BH)
          if (isTRUE(enable_depensation) && eggs_prev < 0.2 * SSB0)
            R_BH    <- R_BH * (eggs_prev / (0.2 * SSB0)) ^ 2
          Rcapacity[i] <- if (rec_cv == 0) max(1, R_BH) else
            max(1, R_BH * rlnorm(1, 0, sd = sigmaR))
        }

        age_survive       <- age_len * Survival_mat
        age_len           <- grow(age_survive)
        age_len[1, ]      <- Rcapacity[i] * recruit_dist
        N_cur             <- colSums(age_len)
        eggs_cur          <- sum(N_cur * Fec_bins)

        if (i >= last_50_start) {
          yld_i       <- sum(harvest_weight_bins * N_cur)
          SPR_acc     <- SPR_acc     + eggs_cur / SPR_denom
          Yield_acc   <- Yield_acc   + yld_i
          Prop_acc    <- Prop_acc    + sum(trophyvul_bins * N_cur) / max(1, sum(N_cur))
          Recruit_acc <- Recruit_acc + Rcapacity[i]
          hb_tot      <- sum(harvest_number_bins * N_cur)
          if (hb_tot > 0) {
            harv_len_sum <- harv_len_sum +
              sum(harvest_number_bins * N_cur * bin_midpoints) / hb_tot
            harv_len_n   <- harv_len_n + 1L
          }
        }
        eggs_prev <- eggs_cur
      }

      sim_df$SPR[k]               <- SPR_acc     / n50
      # Ratio of sums, not mean of annual ratios: averaging yield_t / R_t
      # inflates YPR by about (1 + rec_cv^2) under lognormal recruitment,
      # because yield_t comes mostly from earlier cohorts.
      sim_df$YPR[k]               <- Yield_acc   / max(1, Recruit_acc)
      sim_df$Prop[k]              <- Prop_acc    / n50
      sim_df$Recruit[k]           <- Recruit_acc / n50
      sim_df$MeanLengthHarvested[k] <-
        if (harv_len_n > 0) harv_len_sum / harv_len_n else NA_real_
      next
    }

    # ── FULL PATH: collect_full_output = TRUE ──────────────────────────────

    # --- Per-replicate storage ------------------------------------------------
    N      <- matrix(0, Ymax, L_bins)
    age_len <- matrix(0, Amax, L_bins)
    Yield  <- rep(NA_real_, Ymax)
    SPRt   <- rep(NA_real_, Ymax)
    YPR    <- rep(NA_real_, Ymax)
    Prop   <- rep(NA_real_, Ymax)
    eggs_t <- rep(NA_real_, Ymax)

    # --- Restore cached burn-in state (computed once outside the k-loop) ------
    N[seq_len(burnin_years), ] <- N_burnin
    age_len                    <- burnin_age_len
    SPR_denom                  <- SPR_denom_ref
    SSB0                       <- SSB0_ref

    # --- Stochastic recruitment capacity for the fished period ---------------
    if (isTRUE(enable_ddr)) {
      Rcapacity <- rep(NA_real_, Ymax)
    } else if (rec_cv == 0) {
      Rcapacity <- rep(Ro, Ymax)
    } else {
      Rcapacity <- Ro * rlnorm(Ymax, meanlog = -0.5 * sigmaR^2, sd = sigmaR)
    }

    # --- Annotate burn-in years (U = 0, no harvest) -------------------------
    for (yr in seq_len(burnin_years)) {
      Yield[yr]  <- 0
      eggs_t[yr] <- sum(N[yr, ] * Fec_bins)
      SPRt[yr]   <- eggs_t[yr] / SPR_denom   # approaches 1.0 as burn-in converges
      YPR[yr]    <- 0
      Prop[yr]   <- sum(trophyvul_bins * N[yr, ]) / max(1, sum(N[yr, ]))
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

      age_survive           <- age_len * Survival_mat
      new_age_len           <- grow(age_survive)
      new_age_len[1, ]      <- Rcapacity[i] * recruit_dist
      age_len               <- new_age_len
      N[i, ]                <- colSums(age_len)
      Yield[i]              <- sum(harvest_weight_bins * N[i, ])
      eggs_t[i]        <- sum(N[i, ] * Fec_bins)

      # SPR = stock egg production relative to the unfished equilibrium.
      # Values > 1 occur in favourable recruitment years (biologically meaningful).
      SPRt[i]          <- eggs_t[i] / SPR_denom
      Prop[i]          <- sum(trophyvul_bins * N[i, ]) / max(1, sum(N[i, ]))
    }

    # --- Summarise last 50 fished years into sim_df row ----------------------
    last_50_start <- max(start_year, Ymax - 49L)
    idx           <- last_50_start:Ymax
    sim_df$SPR[k]     <- mean(SPRt[idx], na.rm = TRUE)
    # Ratio of sums over the final 50 years (see fast path comment).
    sim_df$YPR[k]     <- sum(Yield[idx]) / max(1, sum(Rcapacity[idx]))
    # Annual YPR for the time-series plots: yield_t over this replicate's
    # mean recruitment in the final 50 years (not yield_t / R_t, which is
    # biased upward). Its mean over those years equals sim_df$YPR[k].
    fished        <- start_year:Ymax
    YPR[fished]   <- Yield[fished] / max(1, mean(Rcapacity[idx]))
    sim_df$Prop[k]    <- mean(Prop[idx],     na.rm = TRUE)
    sim_df$Recruit[k] <- mean(Rcapacity[idx], na.rm = TRUE)

    harvest_lengths <- vapply(idx, function(yr) {
      hb <- N[yr, ] * harvest_number_bins
      th <- sum(hb)
      if (th > 0) sum(hb * bin_midpoints) / th else NA_real_
    }, numeric(1))
    harvest_mh <- mean(harvest_lengths, na.rm = TRUE)
    sim_df$MeanLengthHarvested[k] <- if (is.nan(harvest_mh)) NA_real_ else harvest_mh

    if (collect_full_output) {
      all_YPR[, k]          <- YPR
      all_SPR[, k]          <- SPRt
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
    out$all_Prop         <- all_Prop
    out$all_EggProd      <- all_EggProd
    out$all_Abundance    <- all_Abundance
    out$all_AgeAbundance <- all_AgeAbundance
  }
  out
}
