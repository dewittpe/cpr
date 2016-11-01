# Version 0.1.1.9000

## New Features
* Option to save fits in `cnr` (#8)
* Option to define the number of polynomial coefficients to use in `cnr` (#10)

## Bug Fixes
* `from` and `to` args for `plot.cpr_cpr` fixed (#14)

# Version 0.1.1

## New Features
* `plot.cpr_cp` allows the user to suppress the plotting of the control polygon.
  When plotting multiple control polygons and splines, this option will make it
  easier to view the spline functions.

# Non-visible changes:
* Extended testing scripts.

# Version 0.1.0

**First version of univariable cpr methods ready for deployment**

## Big picture
`cpr::cp` and `cpr::cpr` have been used for the simulations which are aimed to
be part of the first manuscript.  Modifications might be needed, but hopefully
the univariable methods are stable.

A lot of changes in the implimentation and API have occured from the 0.0.x
series.  The aim for version 0.2.0 will be to have a very similar API for
`cpr::cn` and `cpr::cnr` as provided for the `cpr::cp` and `cpr::cpr` calls.


# version 0.0.5

## New Features
* First and second derivatives of B-splines via `bsplineD`

## Extended Documentation
* Examples added to `bsplines`

## End User non-visible changes:
* Added the not-to-be-exported function `generate_cp_data`
* Redesign of the deboor.cpp file so that the bsplines are accessable.  The
  prior design only allowed access to the basis, the current design allows
  access to the generic B-splines.


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
* Removed the background vignette... to much detail right now, too much time
  required to build and install the package.

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
