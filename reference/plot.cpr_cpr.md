# Control Polygon Reduction Plots

A wrapper around several ggplot2 calls to help evaluate results of a CPR
run.

## Usage

``` r
# S3 method for class 'cpr_cpr'
plot(x, from = 1, to, ...)
```

## Arguments

- x:

  a `cpr_cpr` object

- from:

  the first index of `x` to plot

- to:

  the last index of `x` to plot

- ...:

  arguments passed to `plot.cpr_cp`

## Value

a `ggplot` object

## See also

[`plot.cpr_cp`](http://www.peteredewitt.com/cpr/reference/plot.cpr_cp.md),
[`cpr`](http://www.peteredewitt.com/cpr/reference/cpr.md),
[`cp`](http://www.peteredewitt.com/cpr/reference/cp.md)

## Examples

``` r
set.seed(42)
x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 100)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))

initial_cp0 <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6))
     , data = DF
     , keep_fit = TRUE # default is FALSE
  )
cpr0 <- cpr(initial_cp0)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==========                                                            |  14%  |                                                                              |====================                                                  |  29%  |                                                                              |==============================                                        |  43%  |                                                                              |========================================                              |  57%  |                                                                              |==================================================                    |  71%  |                                                                              |============================================================          |  86%  |                                                                              |======================================================================| 100%

plot(cpr0)
#> Error in eval(expr): object 'cpr0' not found
plot(cpr0, show_spline = TRUE, show_cp = FALSE, color = TRUE, from = 2, to = 4)
#> Error in eval(expr): object 'cpr0' not found
```
