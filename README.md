# Small Sample Size Species Distribution Modelling

## The S4DM R package
This repository contains an R package that implements Species Distribution Modelling methods which work even when there are relatively few occurrence records (as is the case for poorly-sample or range-restricted species). These methods were primarily developed by the Drake lab, and include three types of methods: 1) Plug-and-play models, 2) environmental-range models, and 3) density-ratio models. Most of the important functions in this package are wrappers around existing functions that handle density estimation or density-ratio estimation.  Much of this code was created by modifying existing code at https://github.com/DrakeLab/PlugNPlay in order to make functions more modular and extensible.

## How it works
The package is build on a hierarchy of modular functions, each of which calls on lower-level functions:

1. The highest-level functions are `make_range_map` and `evaluate_range_map`, which are wrappers for...
2. The next-highest-level functions, `fit_plug_and_play`, `fit_density_ratio`, `project_plug_and_play`, and `project_density_ratio`, which are wrappers for ...
3. Internal modules such as `pnp_kde` or `dr_ulsif`.  These modules both model the environmental covariates and predict values at environmental covariates from fitted models.  These modules are largely wrappers around existing functions for fitting density functions or density-ratios.  Modules beginning with "pnp_" pertain to density functions while models beginning with "dr_" pertain to density ratio functions.

This hierarchical structure built on low-level internal modules is designed to allow for the easy addition of new methods by adding small, self-contained modules. The highest-level functions are intended only for quick-and-dirty analyses or quick visualizations.  We recommend that users focus on the "fit" and "project" functions for work intended for publication.

### What is Plug-and-Play?
In general usage, the term plug-and-play (PNP) refers to software or hardware that can be connected without any additional setup or configuration.  In the context of species distribution models, plug-and-play is a framework developed by Drake and Richards (https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2373) that recognizes that species distribution models can be constructed by "plugging in" any methods that can estimate density functions.
