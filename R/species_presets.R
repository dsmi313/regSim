get_species_preset <- function(species) {
  presets <- list(
    white_crappie = list(
      wl_a = 2.40991e-6, wl_b = 3.38, mat_size = 180, memorable_size = 305,
      linf = 353, vbk = 0.374, t0 = 0.197, nat_mort = 0.374,
      rec_cv = 0.8, amax = 8, ymax = 128, capsize = 204,
      fec_exp = 1.27,
      label = "Loaded White Crappie parameters (Smith et al. 2025)"
    ),
    black_crappie = list(
      wl_a = 1.10e-5, wl_b = 3.07, mat_size = 180, memorable_size = 305,
      linf = 381, vbk = 0.19, t0 = 0.34, nat_mort = 0.19,
      rec_cv = 0.8, amax = 8, ymax = 128, capsize = 204,
      fec_exp = 1.27,
      label = "Loaded Black Crappie parameters (FishBase median)"
    ),
    walleye = list(
      wl_a = 6.63e-6, wl_b = 3.10, mat_size = 356, memorable_size = 635,
      harvlim = 356,
      linf = 683, vbk = 0.32, t0 = -0.52, nat_mort = 0.32,
      rec_cv = 1.1, amax = 15, ymax = 135, capsize = 330,
      fec_exp = 1.18,
      label = "Loaded Walleye parameters (FishBase median)"
    ),
    lmb = list(
      wl_a = 8.16e-6, wl_b = 3.10, mat_size = 203, memorable_size = 508,
      harvlim = 305,
      linf = 584, vbk = 0.22, t0 = 0, nat_mort = 0.22,
      rec_cv = 0.5, amax = 12, ymax = 132, capsize = 280,
      fec_exp = 1.18,
      label = "Loaded Largemouth Bass parameters (FishBase median)"
    ),
    smb = list(
      wl_a = 1.09e-5, wl_b = 3.08, mat_size = 254, memorable_size = 432,
      harvlim = 305,
      linf = 525, vbk = 0.17, t0 = -0.33, nat_mort = 0.17,
      rec_cv = 0.7, amax = 12, ymax = 132, capsize = 280,
      fec_exp = 1.18,
      label = "Loaded Smallmouth Bass parameters (FishBase median)"
    ),
    channel_catfish = list(
      wl_a = 1.66e-6, wl_b = 3.30, mat_size = 356, memorable_size = 711,
      harvlim = 305,
      linf = 592, vbk = 0.17, t0 = -0.62, nat_mort = 0.17,
      rec_cv = 0.4, amax = 24, ymax = 144, capsize = 300,
      fec_exp = 1.18,
      label = "Loaded Channel Catfish parameters (FishBase median)"
    ),
    blue_catfish = list(
      wl_a = 7.74e-7, wl_b = 3.41, mat_size = 350, memorable_size = 889,
      harvlim = 305,
      linf = 1300, vbk = 0.079, t0 = -1.3, nat_mort = 0.15,
      rec_cv = 0.5, amax = 30, ymax = 150, capsize = 300,
      fec_exp = 1.18,
      label = "Loaded Blue Catfish parameters (FishBase median)"
    )
  )
  presets[[species]]
}
