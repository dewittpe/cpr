# Get the Control Polygon and the Spline Function

Generate `data.frame`s for interpolating and plotting a spline function,
given a `cpr_cp` or `cpr_cn` object.

## Usage

``` r
get_spline(x, margin = 1, at, n = 100, se = FALSE, derivative = 0)
```

## Arguments

- x:

  a `cpr_cp` or `cpr_cn` object.

- margin:

  an integer identifying the marginal of the control net to slice along.
  Only used when working `x` is a `cpr_cn` object.

- at:

  point value for marginals not defined in the `margin`. Only used when
  `x` is a `cpr_cn` object. Expected input is a list of length
  `length(attr(x, "bspline_list"))`. Entries for elements `marginal` are
  ignored. If omitted, the midpoint between the boundary knots for each
  marginal is used.

- n:

  the length of sequence to use for interpolating the spline function.

- se:

  if `TRUE` return the estimated standard error for the spline or the
  derivative.

- derivative:

  A value of 0 (default) returns the spline, 1 the first derivative, 2
  the second derivative.

## Value

a `data.frame` `n` rows and two columns `x` and `y`, the values for the
spline. A third column with the standard error is returned if requested.

## Details

A control polygon, `cpr\_cp` object, has a spline function f(x).
`get_spline` returns a list of two `data.frame`. The `cp` element is a
`data.frame` with the (x, y) coordinates control points and the `spline`
element is a `data.frame` with `n` rows for interpolating f(x).

For a control net, `cpr\_cn` object, the return is the same as for a
`cpr\_cp` object, but conceptually different. Where a `cpr\_cp` objects
have a uni-variable spline function, `cpr\_cn` have multi-variable
spline surfaces. `get_spline` returns a "slice" of the higher
dimensional object. For example, consider a three-dimensional control
net defined on the unit cube with marginals `x1`, `x2`, and `x3`. The
implied spline surface is the function f(x1, x2, x3).
`get_spline(x, margin = 2, at = list(0.2, NA, 0.5))` would return the
control polygon and spline surface for f(0.2, x, 0.5).

See
[`get_surface`](http://www.peteredewitt.com/cpr/reference/get_surface.md)
for taking a two-dimensional slice of a three-plus dimensional control
net, or, for generating a useful data set for plotting the surface of a
two-dimensional control net.

## See also

[`get_surface`](http://www.peteredewitt.com/cpr/reference/get_surface.md)

## Examples

``` r
data(spdg, package = "cpr")

## Extract the control polygon and spline for plotting.  We'll use base R
## graphics for this example.
a_cp <- cp(pdg ~ bsplines(day, df = 10, bknots = c(-1, 1)), data = spdg)

spline <- get_spline(a_cp)
plot(spline$x, spline$y, type = "l")


# compare to the plot.cpr_cp method
plot(a_cp, show_spline = TRUE)


# derivatives
f0 <- function(x) {
  #(x + 2) * (x - 1) * (x - 3)
  x^3 - 2 * x^2 - 5 * x + 6
}
f1 <- function(x) {
  3 * x^2 - 4 * x - 5
}
f2 <- function(x) {
  6 * x - 4
}

x <- sort(runif(n = 100, min = -3, max = 5))
bknots = c(-3, 5)
bmat <- bsplines(x, bknots = bknots)
theta <- coef(lm(f0(x) ~ bsplines(x, bknots = bknots) + 0) )

cp0 <- cp(bmat, theta)
spline0 <- get_spline(cp0, derivative = 0)
spline1 <- get_spline(cp0, derivative = 1)
spline2 <- get_spline(cp0, derivative = 2)

old_par <- par()

par(mfrow = c(1, 3))
plot(x, f0(x), type = "l", main = "spline")
points(spline0$x, spline0$y, pch = 2, col = 'blue')

plot(x, f1(x), type = "l", main = "first derivative")
points(spline1$x, spline1$y, pch = 2, col = 'blue')

plot(x, f2(x), type = "l", main = "second derivative")
points(spline2$x, spline2$y, pch = 2, col = 'blue')


par(old_par)
#> Warning: graphical parameter "cin" cannot be set
#> Warning: graphical parameter "cra" cannot be set
#> Warning: graphical parameter "csi" cannot be set
#> Warning: graphical parameter "cxy" cannot be set
#> Warning: graphical parameter "din" cannot be set
#> Warning: graphical parameter "page" cannot be set
```
