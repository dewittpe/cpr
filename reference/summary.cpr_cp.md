# Summarize a Control Polygon Object

Summarize a Control Polygon Object

## Usage

``` r
# S3 method for class 'cpr_cp'
summary(object, wiggle = TRUE, integrate.args = list(), ...)
```

## Arguments

- object:

  a `cpr_cp` object

- wiggle:

  logical, if `TRUE` then the integral of the squared second derivative
  of the spline function will be calculated via
  [`integrate`](https://rdrr.io/r/stats/integrate.html).

- integrate.args:

  a list of arguments passed to
  [`wiggle`](http://www.peteredewitt.com/cpr/reference/wiggle.md) and
  ultimately [`integrate`](https://rdrr.io/r/stats/integrate.html).

- ...:

  pass through

## Value

a `cpr_summary_cpr_cp` object, that is just a `data.frame`

## Examples

``` r
set.seed(42)
x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 100)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))

initial_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6))
     , data = DF
     , keep_fit = TRUE # default is FALSE
  )

summary(initial_cp)
#>   dfs n_iknots       iknots    loglik      rss       rse   wiggle fdsc
#> 1  10        6 1, 1.5, .... -19.67424 8.677886 0.3105172 87.35118    4
```
