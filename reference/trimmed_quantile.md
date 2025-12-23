# Trimmed Quantiles

For data \\X = x_1, x_2, \ldots, x_n\\, with order statistics
\\x\_{(1)}, x\_{(2)}, \ldots, x\_{(r)}\\ return the quantiles for a
trimmed data set, e.g., \\\boldsymbol{X} \backslash \\x\_{(1)},
x\_{(r)}\\\\ (trim = 1), or \\\boldsymbol{X} \backslash \\x\_{(1)},
x\_{(2)}, x\_{(r-1)}, x\_{(r)}\\\\ (trim = 2).

## Usage

``` r
trimmed_quantile(x, trim = 1L, use_unique = TRUE, ...)
```

## Arguments

- x:

  a numeric vector

- trim:

  defaults to 1, omitting the min and the max

- use_unique:

  logical, if true (defaults), base the quantiles on unique values, if
  false, base the quantiles on all data, after trimming.

- ...:

  other arguments to pass to stats::quantile

## Value

a numeric vector, the return from
[`quantile`](https://rdrr.io/r/stats/quantile.html)

## See also

[`quantile`](https://rdrr.io/r/stats/quantile.html)

## Examples

``` r
trimmed_quantile(1:100, prob = 1:23 / 24, name = FALSE)
#>  [1]  6.041667 10.083333 14.125000 18.166667 22.208333 26.250000 30.291667
#>  [8] 34.333333 38.375000 42.416667 46.458333 50.500000 54.541667 58.583333
#> [15] 62.625000 66.666667 70.708333 74.750000 78.791667 82.833333 86.875000
#> [22] 90.916667 94.958333

# Warning
# trimmed_quantile(1:100, trim = .3, prob = 1:23 / 24, name = FALSE)

# no warning
trimmed_quantile(1:100, trim = 3, prob = 1:23 / 24, name = FALSE)
#>  [1]  7.875 11.750 15.625 19.500 23.375 27.250 31.125 35.000 38.875 42.750
#> [11] 46.625 50.500 54.375 58.250 62.125 66.000 69.875 73.750 77.625 81.500
#> [21] 85.375 89.250 93.125
```
