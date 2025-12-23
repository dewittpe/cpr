# Sign Changes

Count the number of times the first, or second, derivative of a spline
changes sign.

## Usage

``` r
sign_changes(
  object,
  lower = min(object$bknots),
  upper = max(object$bknots),
  n = 1000,
  derivative = 1L,
  ...
)
```

## Arguments

- object:

  a `cpr_cp` object

- lower:

  the lower limit of the integral

- upper:

  the upper limit of the integral

- n:

  number of values to assess the derivative between `lower` and `upper`.

- derivative:

  integer value denoted first or second derivative

- ...:

  pass through

## Value

the number of times the sign of the first or second derivative changes
within the specified interval.

## See also

[`wiggle`](http://www.peteredewitt.com/cpr/reference/wiggle.md)

## Examples

``` r
xvec <- seq(0, 6, length = 500)

# Define the basis matrix
bmat1 <- bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5))
#> Warning: At least one x value >= max(bknots)
bmat2 <- bsplines(x = xvec)
#> Warning: At least one x value >= max(bknots)

# Define the control vertices ordinates
theta1 <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
theta2 <- c(1, 3.4, -2, 1.7)

# build the two control polygons
cp1 <- cp(bmat1, theta1)
cp2 <- cp(bmat2, theta2)
plot(cp1, cp2, show_cp = FALSE, show_spline = TRUE)
#> Warning: Removed 21 rows containing missing values or values outside the scale range
#> (`geom_rug()`).


sign_changes(cp1)
#> [1] 4
sign_changes(cp2)
#> [1] 2

```
