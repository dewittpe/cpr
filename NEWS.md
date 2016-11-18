# Version 0.2.0.9000
Documentation improvements.

## New Features
* `influence_of` and `plot.cpr_influence_of` provide a clean interface for users
  to explore the influence of a set of knots on a spline function.  (#19)
* `color` (`TRUE`/`FALSE`) option added to `plot.cpr_bs`.

# Version 0.2.0
This version has a fairly polished set of tools for b-splines, cpr, and cnr.
This version seems to be in a good place for use in the three major papers 

1. Methods 1: uni-variable functions,
2. Methods 2: multi-variable functions, and
3. Software paper.

Continued development should be focused on bug fixes and minor enhancements.

## New Features
* Option to save fits in `cnr` (#8)
* Option to define the number of polynomial coefficients to use in `cnr` (#10)
* x-axis tick label options for plotting b-splines (#12)
* added `show_xi` to `cpr:::plot.cp` and using `ggplot2::geom_rug` to show the
  location of the knots for each of the control polygons plotted.
* `summary` for `cpr_cn` and `cpr_cnr` objects added.
* `plot` method for `cpr_cnr` objects.
* `margin` option in `cnr` allows the user to specify which marginals CNR will
  be applied to.
* Using `sec.axis` option from `ggplot2_2.2.0` for the plotting of the knot
  sequence and numeric values in `plot.cpr_bs` (#18)

## Bug Fixes
* `from` and `to` args for `plot.cpr_cpr` fixed (#14)
* correct construction of missing `iknots` argument in `btensor`
* `keep` is correctly handled in the `cnr` call.
* `show_xi` correctly handled in the `plot.cpr_cp` call.

## Non-User visible changes
* non-exported function `knot_expr` created to help with plotting the knot
  locations in `cpr:::plot.cpr_bs`.

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

A lot of changes in the implementation and API have occurred from the 0.0.x
series.  The aim for version 0.2.0 will be to have a very similar API for
`cpr::cn` and `cpr::cnr` as provided for the `cpr::cp` and `cpr::cpr` calls.


# version 0.0.5

## New Features
* First and second derivatives of B-splines via `bsplineD`

## Extended Documentation
* Examples added to `bsplines`

## End User non-visible changes:
* Added the not-to-be-exported function `generate_cp_data`
* Redesign of the deboor.cpp file so that the bsplines are accessible.  The
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
