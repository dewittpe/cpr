# Version 0.4.0.9000

## Bug fixes:

* `plot_bs` builds the x-axis correctly for a basis built with values outside of
  the boundary knots.

# Version 0.4.0

## New Features

* `cpr`'s `progress` argument has been extended to control if a progress bar is
  used for just the cpr steps, or if a more detailed progress for of the
  influence weight calculations is reported.

* `influence_of_iknots` gains parallel execution via `pbapply` (#17)

* `plot.cpr_cp` gains the argument `comparitive` which, when set to `FALSE` and
  only one `cpr_cp` is passed in for plotting, the graphic will appear more like
  the `plot.cpr_bs` results.  When `comparitive = TRUE` or more than one
  `cpr_cp` is present, the behavior from v0.3.0 is retained.

* `cp.formula` gains the `methods.args` argument to pass arguments to the
  regression method instead of relying on `...`.

* `d_order_statistic` and `p_order_statistic` were added.  These functions allow
  you to get the density of distribution function for the jth order statistic
  from a sample of size n from a distribution with defined density and
  distribution functions within R.

* `sign_changes` will count the number of sign changes of the first or second
  derivative of a spline function.

* `get_spline` returns standard errors and derivatives (#60)

## User Visible Changes
* `loglikelihood` is not exported in the namespace
* `summary.cpr_cp` now calculates the "wiggle" of the function by default, that
  is, changes the default from `wiggle = FALSE` to `wiggle = TRUE`
* `cp` and `cn` both have the default `keep_fit` argument set to TRUE.  This
  change was made to simplify the prediction methods.
* `print.cpr_bt` returns the object invisibly, it used to return a `str(x)`.
* `print.cpr_cn` returns the object invisibly
* `print.cpr_cnr` returns the object invisibly
* `print.cpr_cpr` returns the object invisibly

## Non-User Visible Changes
* Refactor of c++ defining basis functions, derivatives of basis functions,
  b-splines structures

* `cp.formula` checks the `formula` and requires that `bsplines` is used once
  and is the first term on the right hand side of the formula.

## Defunct Functions
* A major refactor of the internal code as since v0.3.0 has resulted in several
  functions becoming defunct, see `help(cpr-defunct)` for details.

# Version 0.3.0

## New Examples
* `cpr` has examples

## Other Changes
* Depends on Rcpp >= 0.12.11 (actually moved to >= 1.0.11) to handle registering native routines.
* Moves rgl from `Imports` to `Suggests` (re #36)
* Refactoring base code to eliminate the use of dplyr, tidyr, tibble, etc.
  Focus on base R methods to reduce install dependencies and improve long term
  stability of the package.
* Require R > 3.5.0
* Stop using testthat for testing
* Remove use of the tidyr, dplyr
* Improve documentation
* Minor bug fixes
* Replace use of now deprecated `ggplot2::aes_string`

# Version 0.2.3
First public release.

## New Features
* `plot.cpr_cn` supports `rgl` and `plot3D` graphics
* start of a vignette.

# Version 0.2.2

## New Features
* `get_spline` is an S3 method for getting a `data.frame` of interpolated values
  of a spline given a `cpr_cp` object.  Later development will add methods for
  `cpr_cn` objects.
* `predict.cpr_cp` and `predict.cpr_cn` methods added
* `matrix_rank` added
* `update_bsplines` and `update_btensor` methods added (#27)

# Version 0.2.1
Documentation improvements.

## New Features
* `influence_of` and `plot.cpr_influence_of` provide a clean interface for users
  to explore the influence of a set of knots on a spline function.  (#19)
* `color` (`TRUE`/`FALSE`) option added to `plot.cpr_bs`.
* `plot.cpr_cn` lets the user plot 2D surfaces for tensor product surfaces.  The
  plots are for the whole surface if the input is a 2D tensor product, and is a
  2D slice evaluated at a given value for other margins for 3+ dimensional
  tensor products.
* `is.` a collection of `is.cpr_cp`, `is.cpr_bs`, ... functions added.
* The dataset `spdg` has been added to the package.

## Other Changes
* removed a redundant `build_tensor` definition

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
* `from` and `to` arguments for `plot.cpr_cpr` fixed (#14)
* correct construction of missing `iknots` argument in `btensor`
* `keep` is correctly handled in the `cnr` call.
* `show_xi` correctly handled in the `plot.cpr_cp` call.

## Non visible changes
* non-exported function `knot_expr` created to help with plotting the knot
  locations in `cpr:::plot.cpr_bs`.

# Version 0.1.1

## New Features
* `plot.cpr_cp` allows the user to suppress the plotting of the control polygon.
  When plotting multiple control polygons and splines, this option will make it
  easier to view the spline functions.

## Non visible changes
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
* Redesign of the `deboor.cpp` file so that the `bsplines` are accessible.  The
  prior design only allowed access to the basis, the current design allows
  access to the generic B-splines.


# version 0.0.3
Version 0.0.3 is the version of the package used to run the analysis and
simulations presented in the paper submitted to the 28th International
Biometrics Conference, Western North American Region (WNAR) of the Internal
Biometric Society, Student paper competition.  The conference will be held 10 -
16 July 2016 in Victoria, British Columbia, Canada.

## Bug Fixes
* Corrected the attributes calls within `cpr` after adjusting the attributes
  being set on a `cpr_cp`.

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
