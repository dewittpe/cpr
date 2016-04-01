# version 0.0.1.9008

## new features
* Added the function `tensor` for building tensor products of `cpr::bsplines`.
* Added the function `influence_weights` to get the influence weights for each
  internal knot on each marginal of a tensor product.

## Bug Fixes
* trimmed quantile handles the `use_unique` option correctly
* better handeling of ... in `cp()` and `cpr()`

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
