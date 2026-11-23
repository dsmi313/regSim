# regSim 0.2.0

* Growth variability is now a coefficient of variation in length at age
  (default 0.15), replacing the CV on the annual growth increment. At each
  age, length has SD = CV x mean length at age, so absolute spread grows with
  age and individual fish can exceed Linf. `make_growth_matrix()` takes a new
  required `Amax` argument and returns one transition matrix per age
  (an L x L x (Amax - 1) array). `run_population_simulation()` uses the
  age-specific matrices and still accepts a single L x L matrix. The old CV on
  the increment collapsed toward zero near Linf, so no fish could grow past it.
* Harvest slot, protective slot and maximum length edges now use SD = 1% of
  the edge length, like the capture and minimum length curves (they were a
  fixed 0.01 mm). A slot and a minimum sharing a lower limit now treat it the
  same; previously the slot harvested more fish in the bin just above the
  limit.
* Proportion memorable is now memorable-size fish divided by stock-size fish
  (an RSD-M-type index; Gabelhouse 1984), not divided by every fish in the
  population. Species presets gain `stock_size` (Gabelhouse stock lengths in
  inches converted to mm: crappies 127, Walleye 254, Largemouth Bass 203,
  Smallmouth Bass 178, Channel Catfish 279, Blue Catfish 305).
  `make_vulnerability_curves(stock_size =)` returns `stockvul_bins`, which
  `run_population_simulation()`, `run_yield_curve()` and
  `run_uncertainty_simulation()` accept; the app has a Stock Size input.
* Recruitment noise under density-dependent recruitment is now mean-corrected
  (meanlog = -sigma^2 / 2), like the default path. It inflated mean
  recruitment by exp(sigma^2 / 2) (1.28x crappie, 1.49x Walleye, 1.12x
  Largemouth Bass). The case studies don't use DDR.
* App help text for steepness had its meaning reversed and now reads
  correctly; the depensation text now gives the actual formula.
* `make_length_bins()` spans 0 to 1.6 x Linf by default (was 1.2) so the
  length grid holds fish larger than Linf; `max_mult = 1.2` restores the old
  grid.
* Case-study scripts run growth CV 0.10, 0.15 and 0.20 as a sensitivity
  factor and tag results with `growth_cv`; `manuscript_figures.R` plots
  CV = 0.15 and writes all three to Supplementary Table S1.
* YPR is now total yield over the final 50 years divided by total recruits
  over the same years. The previous mean of annual yield / recruits ratios
  inflated YPR by about 1 + rec_cv^2 under stochastic recruitment. The app's
  YPR time series now averages to the reported YPR (#53).
* Largemouth Bass and Smallmouth Bass presets use fixed species-median natural
  mortality from Beamesderfer and North (1995): M = 0.46 and 0.43, held
  constant across growth scenarios, in place of M = 1.5K (#52).
* "Traditional slot" is now "harvest slot" throughout the app, code and
  scripts (`slot_type = "harvest"`). `slot_type = "traditional"` still works
  as the old name.
* Adds `scripts/manuscript_figures.R`, which builds manuscript Figures 1-6
  and Supplementary Table S1 from the case-study results.
* Case-study scripts set a fixed seed so reruns reproduce the published
  numbers.

# regSim 0.1.0

* Initial version.
