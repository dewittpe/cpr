# Determine the (quasi) Log Likelihood for a regression object.

Return, via [`logLik`](https://rdrr.io/r/stats/logLik.html) or a custom
S3 method, the (quasi) log likelihood of a regression object.

## Usage

``` r
loglikelihood(x, ...)
```

## Arguments

- x:

  a regression fit object

- ...:

  passed through to [`logLik`](https://rdrr.io/r/stats/logLik.html)

## Value

the numeric value of the (quasi) log likelihood.

## Details

This function is used by `cpr` and `cnr` to determine the (quasi) log
likelihood returned in the `cpr_cpr` and `cpr_cnr` objects.

Generally this function defaults to
[`logLik`](https://rdrr.io/r/stats/logLik.html). Therefore, if an S3
method for determining the (quasi) log likelihood exists in the
workspace everything should work. If an S3 method does not exist you
should define one.

See `methods(loglikelihood)` for a list of the provided methods. The
default method uses [`logLik`](https://rdrr.io/r/stats/logLik.html).

## See also

[`cpr`](http://www.peteredewitt.com/cpr/reference/cpr.md)
[`cnr`](http://www.peteredewitt.com/cpr/reference/cnr.md)
[`logLik`](https://rdrr.io/r/stats/logLik.html)

## Examples

``` r
fit <- lm(mpg ~ wt, data = mtcars)
stats::logLik(fit)
#> 'log Lik.' -80.01471 (df=3)
cpr:::loglikelihood(fit)
#> [1] -80.01471
```
