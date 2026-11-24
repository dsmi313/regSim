#' Look up growth parameter presets for a species and growth scenario
#'
#' Returns von Bertalanffy growth parameters and the matching natural-mortality
#' default for a given species under a slow, moderate, or fast growth scenario.
#'
#' Natural mortality defaults follow species-specific conventions:
#' \itemize{
#'   \item Crappie (white, black): \eqn{M = K} (Pauly 1980 approximation).
#'   \item Largemouth and smallmouth bass: fixed literature instantaneous M
#'     (0.46 and 0.43, respectively) from Beamesderfer and North (1995, N. Am.
#'     J. Fish. Manage. 15:688-704, Table 2). These are the Table 2 means; the
#'     medians are 0.42 and 0.39. The same M is used for northern and
#'     southern Largemouth Bass. A growth-based default is
#'     deliberately \emph{not} used for these two species (see the note at the
#'     \code{lmb_north}/\code{lmb_south}/\code{smb} entries below).
#'   \item All other species: \eqn{M = 1.5 K}, the median ratio reported for
#'     North American freshwater fishes by Jensen (1996, Can. J. Fish. Aquat.
#'     Sci. 53:820-822).
#' }
#'
#' @param species Species key, one of \code{"white_crappie"},
#'   \code{"black_crappie"}, \code{"walleye"}, \code{"lmb_north"},
#'   \code{"lmb_south"}, \code{"smb"},
#'   \code{"channel_catfish"}, or \code{"blue_catfish"}.
#' @param preset Growth scenario, one of \code{"slow"}, \code{"moderate"}, or
#'   \code{"fast"}.
#'
#' @return A named list with \code{linf}, \code{vbk}, \code{t0}, and
#'   \code{nat_mort}, or \code{NULL} if the species/preset combination is not
#'   defined (e.g. \code{"custom"}).
#' @export
get_growth_preset <- function(species, preset) {
  presets <- list(
    # Crappie: M = K (Pauly 1980)
    white_crappie = list(
      slow     = list(linf = 333, vbk = 0.325, t0 =  0.174, nat_mort = 0.325),
      moderate = list(linf = 353, vbk = 0.374, t0 =  0.197, nat_mort = 0.374),
      fast     = list(linf = 356, vbk = 0.691, t0 = -0.056, nat_mort = 0.691)
    ),
    black_crappie = list(
      slow     = list(linf = 440, vbk = 0.17, t0 = 0.34, nat_mort = 0.17),
      moderate = list(linf = 381, vbk = 0.19, t0 = 0.34, nat_mort = 0.19),
      fast     = list(linf = 356, vbk = 0.26, t0 = 0.34, nat_mort = 0.26)
    ),
    # Non-crappie species: M = 1.5 * K (Jensen 1996, CJFAS 53:820-822)
    walleye = list(
      # K = 25th/50th/75th percentiles of FishBase records (42 not flagged
      # questionable, pulled Nov 23, 2026); Linf = 75th/50th/25th (inverse pairing)
      slow     = list(linf = 750, vbk = 0.25, t0 = -0.66, nat_mort = 0.25 * 1.5),
      moderate = list(linf = 683, vbk = 0.31, t0 = -0.52, nat_mort = 0.31 * 1.5),
      fast     = list(linf = 622, vbk = 0.39, t0 = -0.20, nat_mort = 0.39 * 1.5)
    ),
    # Black bass (lmb_north, lmb_south, smb): fixed M from Beamesderfer and North
    # (1995, NAJFM 15:688-704, Table 2 means), NOT the Jensen M = 1.5 K default used
    # for the other non-crappie species. Jensen's 1.5 coefficient assumes a
    # growth-mortality correlation that B&N found absent for both largemouth
    # (N = 698) and smallmouth (N = 409) bass; they showed growth-based M
    # regressions run 23-35% low for these species and are not appropriate for
    # point estimates. Idaho catch-curve data (Frawley, IDFG) imply M/K ratios
    # of 4.3-7.2 across three populations, far outside Jensen's applicable
    # range. M is held constant across the slow/moderate/fast growth scenarios
    # precisely because it does not scale with K for these species; K and M
    # remain fully independent, user-overridable inputs.
    # Largemouth Bass has separate northern and southern presets
    # (inst/extdata/lmb_growth_presets.xlsx lists every record). Region: South =
    # 37 N or below, North = above 37 N (New Mexico kept North for its 1,396 m
    # elevation). Linf and K pools: Beamesderfer and North (1995, Table A.2)
    # state means (the same values FishBase lists), plus single-lake fits
    # (North: Lake Rebecca MN and a second Ontario FishBase record; South:
    # Allen and Pine 2000, Pullen 2024, Lakes Rhodhiss and Glenville NC, and
    # six Florida lakes refit from Porak et al. 1986). North n = 26, South n = 21.
    # Slow = 75th pct Linf with 25th pct K; moderate = medians; fast = 25th pct
    # Linf with 75th pct K (inverse pairing). t0 uses the same ordering (slow =
    # 25th pct, fast = 75th pct) from records with an estimated t0 only; state
    # t0 values come from free-t0 refits of Beamesderfer and North Table A.1
    # mean lengths at age (North n = 25, South n = 22).
    lmb_north = list(
      slow     = list(linf = 687, vbk = 0.143, t0 = -0.15, nat_mort = 0.46),
      moderate = list(linf = 591, vbk = 0.185, t0 =  0.05, nat_mort = 0.46),
      fast     = list(linf = 556, vbk = 0.220, t0 =  0.18, nat_mort = 0.46)
    ),
    lmb_south = list(
      slow     = list(linf = 666, vbk = 0.160, t0 = -0.75, nat_mort = 0.46),
      moderate = list(linf = 619, vbk = 0.254, t0 = -0.33, nat_mort = 0.46),
      fast     = list(linf = 563, vbk = 0.280, t0 = -0.07, nat_mort = 0.46)
    ),
    smb = list(
      slow     = list(linf = 608, vbk = 0.14, t0 = -0.45, nat_mort = 0.43),
      moderate = list(linf = 525, vbk = 0.17, t0 = -0.33, nat_mort = 0.43),
      fast     = list(linf = 506, vbk = 0.22, t0 =  0.02, nat_mort = 0.43)
    ),
    channel_catfish = list(
      slow     = list(linf = 797, vbk = 0.12, t0 = -0.82, nat_mort = 0.12 * 1.5),
      moderate = list(linf = 592, vbk = 0.17, t0 = -0.62, nat_mort = 0.17 * 1.5),
      fast     = list(linf = 470, vbk = 0.23, t0 = -0.20, nat_mort = 0.23 * 1.5)
    ),
    blue_catfish = list(
      slow     = list(linf = 1396, vbk = 0.051, t0 = -1.52, nat_mort = 0.051 * 1.5),
      moderate = list(linf = 1300, vbk = 0.079, t0 = -1.30, nat_mort = 0.079 * 1.5),
      fast     = list(linf = 1060, vbk = 0.095, t0 = -1.01, nat_mort = 0.095 * 1.5)
    )
  )

  sp <- presets[[species]]
  if (is.null(sp)) return(NULL)
  sp[[preset]]
}
