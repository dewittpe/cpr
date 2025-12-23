# Model Prediction

Model prediction for `cpr_cp` and `cpr_cn` objects.

## Usage

``` r
# S3 method for class 'cpr_cp'
predict(object, ...)
```

## Arguments

- object:

  a `cpr_cp` or `cpr_cn` object

- ...:

  passed to [`predict`](https://rdrr.io/r/stats/predict.html)

## Value

the same as you would get from calling
[`predict`](https://rdrr.io/r/stats/predict.html) on the `object$fit`.

## Examples

``` r
acp <- cp(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53))
           , data = spdg
          , keep_fit = TRUE)
acp_pred0 <- predict(acp$fit, se.fit = TRUE)
acp_pred <- predict(acp, se.fit = TRUE)
all.equal(acp_pred0, acp_pred)
#> [1] TRUE
```
