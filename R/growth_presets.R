#' Look up growth parameter presets for a species and growth scenario
#'
#' Returns von Bertalanffy growth parameters and the matching natural-mortality
#' default for a given species under a slow, moderate, or fast growth scenario.
#'
#' Natural mortality defaults follow two conventions:
#' \itemize{
#'   \item Crappie (white, black): \eqn{M = K} (Pauly 1980 approximation).
#'   \item All other species: \eqn{M = 1.5 K}, the median ratio reported for
#'     North American freshwater fishes by Jensen (1996, Can. J. Fish. Aquat.
#'     Sci. 53:820-822).
#' }
#'
#' @param species Species key, one of \code{"white_crappie"},
#'   \code{"black_crappie"}, \code{"walleye"}, \code{"lmb"}, \code{"smb"},
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
      slow     = list(linf = 748, vbk = 0.24, t0 = -0.66, nat_mort = 0.24 * 1.5),
      moderate = list(linf = 683, vbk = 0.32, t0 = -0.52, nat_mort = 0.32 * 1.5),
      fast     = list(linf = 615, vbk = 0.43, t0 = -0.20, nat_mort = 0.43 * 1.5)
    ),
    lmb = list(
      slow     = list(linf = 638, vbk = 0.17, t0 = -0.21, nat_mort = 0.17 * 1.5),
      moderate = list(linf = 584, vbk = 0.22, t0 =  0.00, nat_mort = 0.22 * 1.5),
      fast     = list(linf = 540, vbk = 0.28, t0 =  0.10, nat_mort = 0.28 * 1.5)
    ),
    smb = list(
      slow     = list(linf = 608, vbk = 0.14, t0 = -0.45, nat_mort = 0.14 * 1.5),
      moderate = list(linf = 525, vbk = 0.17, t0 = -0.33, nat_mort = 0.17 * 1.5),
      fast     = list(linf = 506, vbk = 0.22, t0 =  0.02, nat_mort = 0.22 * 1.5)
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
