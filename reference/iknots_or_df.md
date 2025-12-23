# Internal Knots or Degrees of Freedom

Check order, degrees of freedom (df) and iknots

## Usage

``` r
iknots_or_df(x, iknots, df, order)
```

## Arguments

- x:

  the support - a numeric vector

- iknots:

  internal knots - a numeric vector

- df:

  degrees of freedom - a numeric value of length 1

- order:

  polynomial order

## Value

a numeric vector to use as the internal knots defining a B-spline.

## Details

This is an internal function, not to be exported, and used in the calls
for [`bsplines`](http://www.peteredewitt.com/cpr/reference/bsplines.md)
and [`bsplineD`](http://www.peteredewitt.com/cpr/reference/bsplineD.md).

Use `iknots` preferentially. If iknots are not provided then return the
[`trimmed_quantile`](http://www.peteredewitt.com/cpr/reference/trimmed_quantile.md)
for the appropriate `df` and `order`

## See also

[`bsplines`](http://www.peteredewitt.com/cpr/reference/bsplines.md),
[`bsplineD`](http://www.peteredewitt.com/cpr/reference/bsplineD.md),
[`trimmed_quantile`](http://www.peteredewitt.com/cpr/reference/trimmed_quantile.md)

## Examples

``` r
xvec <- runif(600, min = 0, max = 3)

# return the iknots
cpr:::iknots_or_df(x = xvec, iknots = 1:2, df = NULL, order = NULL)
#> [1] 1 2

# return the iknots even when the df and order are provided
cpr:::iknots_or_df(x = xvec, iknots = 1:2, df = 56, order = 12)
#> Warning: Both iknots and df defined, using iknots
#> [1] 1 2

# return numeric(0) when df <= order (df < order will also give a warning)
cpr:::iknots_or_df(x = xvec, iknots = NULL, df = 6, order = 6)
#> numeric(0)

# return trimmed_quantile when df > order
# probs = (df - order) / (df - order + 1)
cpr:::iknots_or_df(x = xvec, iknots = NULL, df = 10, order = 4)
#> 14.28571% 28.57143% 42.85714% 57.14286% 71.42857% 85.71429% 
#> 0.4391773 0.9016051 1.3163082 1.7285738 2.1503820 2.5602099 
cpr::trimmed_quantile(xvec, probs = 1:6 / 7)
#> 14.28571% 28.57143% 42.85714% 57.14286% 71.42857% 85.71429% 
#> 0.4391773 0.9016051 1.3163082 1.7285738 2.1503820 2.5602099 
```
