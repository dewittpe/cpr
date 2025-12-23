# Control Net Reduction Plots

A collection of function for the inspection and evaluation of the
control polygon reduction.

## Usage

``` r
# S3 method for class 'cpr_cnr'
plot(x, type = "rse", from = 1, to, ...)
```

## Arguments

- x:

  a `cpr_cnr` object

- type:

  type of diagnostic plot. `"loglik"` for the log likelihood by degrees
  of freedom, `"rse"` for residual standard error by model index

- from:

  the first index of `x` to plot

- to:

  the last index of `x` to plot

- ...:

  pass through

## Value

a ggplot

## Examples

``` r
initial_cn <- cn(log10(pdg) ~ btensor(list(day, age)
                        , df = list(10, 8)
                        , bknots = list(c(-1, 1), c(44, 53))
                        )
          , data = spdg)

cnr0 <- cnr(initial_cn)
#>   |                                                                              |                                                                      |   0%  |                                                                              |======                                                                |   9%  |                                                                              |=============                                                         |  18%  |                                                                              |===================                                                   |  27%  |                                                                              |=========================                                             |  36%  |                                                                              |================================                                      |  45%  |                                                                              |======================================                                |  55%  |                                                                              |=============================================                         |  64%  |                                                                              |===================================================                   |  73%  |                                                                              |=========================================================             |  82%  |                                                                              |================================================================      |  91%  |                                                                              |======================================================================| 100%

plot(cnr0)

```
