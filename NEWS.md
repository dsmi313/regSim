# regSim 0.2.0

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
