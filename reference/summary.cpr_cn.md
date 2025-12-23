# Summary of Control Net

Generate a summary of control net object

## Usage

``` r
# S3 method for class 'cpr_cn'
summary(object, ...)
```

## Arguments

- object:

  a `cpr_cn` object

- ...:

  pass through

## Value

a `data.frame`

## Examples

``` r
acn <- cn(log10(pdg) ~ btensor(list(day, age)
                               , df = list(10, 8)
                               , bknots = list(c(-1, 1), c(44, 53)))
         , data = spdg)

summary(acn)
#>   dfs    loglik      rss       rse n_iknots1      iknots1 n_iknots2
#> 1  80 -7761.069 2708.164 0.3321463         6 -0.79187....         4
#>        iknots2
#> 1 47.33451....
```
