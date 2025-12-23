# Control Nets

Generate the control net for a univariate B-spline

## Usage

``` r
cn(x, ...)

# S3 method for class 'cpr_bt'
cn(x, theta, ...)

# S3 method for class 'formula'
cn(
  formula,
  data,
  method = stats::lm,
  method.args = list(),
  keep_fit = TRUE,
  check_rank = TRUE,
  ...
)
```

## Arguments

- x:

  a `cpr_bt` object

- ...:

  pass through

- theta:

  a vector of (regression) coefficients, the ordinates of the control
  net.

- formula:

  a formula that is appropriate for regression method being used.

- data:

  a required `data.frame`

- method:

  the regression method such as [`lm`](https://rdrr.io/r/stats/lm.html),
  [`glm`](https://rdrr.io/r/stats/glm.html),
  [`lmer`](https://rdrr.io/pkg/lme4/man/lmer.html), etc.

- method.args:

  a list of additional arguments to pass to the regression method.

- keep_fit:

  (logical, defaults to `FALSE`). If `TRUE` the regression model fit is
  retained and returned in the `fit` element. If `FALSE` the regression
  model is not saved and the `fit` element will be `NA`.

- check_rank:

  (logical, defaults to `TRUE`) if TRUE check that the design matrix is
  full rank.

## Value

a `cpr_cn` object. This is a list with the following elements. Some of
the elements are omitted when using the `cn.cpr_bt` method.

- cn:

  the control net, `data.frame` with each row defining a vertex of the
  control net

- bspline_list:

  A list of the marginal B-splines

- call:

  the call

- keep_fit:

  logical, indicates if the regression model was retained

- fit:

  if `isTRUE(keep_fit)` then the regression model is here, else `NA`.

- coefficients:

  regression coefficients, only the fixed effects if a mixed effects
  model was used.

- vcov:

  The variance-covariance matrix for the `coefficients`

- loglik:

  The log-likelihood for the regression model

- rse:

  the residual standard error for the regression model

## Details

`cn` generates the control net for the given B-spline function. There
are several methods for building a control net.

## See also

[`summary.cpr_cn`](http://www.peteredewitt.com/cpr/reference/summary.cpr_cn.md),
[`cnr`](http://www.peteredewitt.com/cpr/reference/cnr.md),
[`plot.cpr_cn`](http://www.peteredewitt.com/cpr/reference/plot.cpr_cn.md)
for plotting control nets

## Examples

``` r
acn <- cn(log10(pdg) ~
              btensor(   x     = list(day, age)
                      , df     = list(30, 4)
                      , bknots = list(c(-1, 1), c(44, 53))
              )
           , data = spdg)
#> Warning: the ‘nobars’ function has moved to the reformulas package. Please update your imports, or ask an upstream package maintainter to do so.
#> This warning is displayed once per session.
str(acn, max.level = 1)
#> List of 12
#>  $ cn          :'data.frame':    120 obs. of  3 variables:
#>  $ bspline_list:List of 2
#>  $ call        : language cn(formula = log10(pdg) ~ btensor(x = list(day, age), df = list(30, 4),      bknots = list(c(-1, 1), c(44, 53))), data = spdg)
#>  $ keep_fit    : logi TRUE
#>  $ fit         :List of 12
#>   ..- attr(*, "class")= chr "lm"
#>  $ theta       : num [1:120] -8.419 -0.293 -0.519 -0.181 -0.316 ...
#>  $ coefficients: Named num [1:120] -8.419 -0.293 -0.519 -0.181 -0.316 ...
#>   ..- attr(*, "names")= chr [1:120] "btensor(x = list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(44, 53)))1" "btensor(x = list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(44, 53)))2" "btensor(x = list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(44, 53)))3" "btensor(x = list(day, age), df = list(30, 4), bknots = list(c(-1, 1), c(44, 53)))4" ...
#>  $ vcov        : num [1:120, 1:120] 387.92 -47.33 15.45 -6 2.97 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ vcov_theta  : num [1:120, 1:120] 387.92 -47.33 15.45 -6 2.97 ...
#>  $ loglik      : num -7737
#>  $ rss         : num 2703
#>  $ rse         : num 0.332
#>  - attr(*, "class")= chr [1:3] "cpr_cn" "cpr_cn" "list"
```
