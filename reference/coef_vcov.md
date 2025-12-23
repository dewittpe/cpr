# Extract Regression Coefficients for B-Splines and Tensor Products of B-splines

An S3 method for extracting the regression coefficients of the
`bsplines` and `btensor` terms. By Default this uses
[`stats::coef`](https://rdrr.io/r/stats/coef.html) to extract all the
regression coefficients. A specific method for `lmerMod` objects has
been provided. If you are using a regression method which
[`stats::coef`](https://rdrr.io/r/stats/coef.html) will not return the
regression coefficients, you'll need to define an S3 method for
[`stats::coef`](https://rdrr.io/r/stats/coef.html) to do so.

## Usage

``` r
coef_vcov(fit, theta_idx)
```

## Arguments

- fit:

  a regression model fit

- theta_idx:

  numeric index for the theta related coefficients

## Value

A list with four elements

- theta:

  theta regression coefficients

- coef:

  all regression coefficients

- vcov_theta:

  subsection of variance-covariance matrix pertaining to the theta
  values

- vcov:

  full variance-covariance matrix

## Details

These functions are called in the
[`cp`](http://www.peteredewitt.com/cpr/reference/cp.md) and
[`cn`](http://www.peteredewitt.com/cpr/reference/cn.md) calls.

## See also

[`coef`](https://rdrr.io/r/stats/coef.html)
[`cp`](http://www.peteredewitt.com/cpr/reference/cp.md)
[`cn`](http://www.peteredewitt.com/cpr/reference/cn.md)

## Examples

``` r
cp0 <- cp(log10(pdg) ~ bsplines(day, df = 6, bknots = c(-1, 1)) + age + ttm, data = spdg)
cv <- cpr:::coef_vcov(cp0$fit)

summary(cv)
#>            Length Class  Mode   
#> theta       8     -none- numeric
#> coef        8     -none- numeric
#> vcov_theta 64     -none- numeric
#> vcov       64     -none- numeric
```
