# version 0.0.3
Version 0.0.3 is the version of the package used to run the analysis and
simulations presented in the paper submitted to the 28th International
Biometrics Conference, Western North American Region (WNAR) of the Internal
Biometric Society, Student paper competition.  The conference will be held 10 -
16 July 2016 in Victoria, British Columbia, Canada.

## Bug Fixes
* Corrected the attr calls within `cpr` after adjusting the attributes being set
  on a `cpr_cp`.

* `plot.cpr_bs` correctly displays the indices for the knot sequence.

## End User Visible changes: 
* The knot insertion matrix **W** is accessible to the end user in a new way.
  Names of functions in `boehem.cpp` are cleaner.
* `plot.cpr_cpr` allows user to select either control polygons or sums of
  squared residuals to be plotted.

## Vignettes
* Extending the background vignette

## End User non-visible changes:
* Redundant definition of `greville_sites` removed.

# version 0.0.2

## new features
* Added the function `tensor` for building tensor products of `cpr::bsplines`.
* Added the function `influence_weights` to get the influence weights for each
  internal knot on each marginal of a tensor product.
* `is.cpr_bs` added.
* S3 methods for `cp`

## Bug Fixes
* trimmed quantile handles the `use_unique` option correctly
* better handling of ... in `cp()` and `cpr()`

# version 0.0.1.9003

First usable version with the method based on the 'importance weight' of
internal knots based on reversing the methods presented by Boehm (1980).
Development of metrics and methods for parsing out the preferable models.

Version 0.0.1.9003 was the first stable version for fitting the exact data
model.

## Vignettes

* The beginnings of the control-polygon vignette started (version 0.0.1.9001)

# version 0.0.1
This version was based on the idea that using an angle to reduce the control
polygon was a good idea.  Further literature review and simulations showed
otherwise.  This version is marked for posterity and the cpp functions are going
to be useful in the following versions as well.
