# Update bsplines or btensor calls

Update `cpr_bs` and `cpr_bt` objects alone or within `cpr_cp` and
`cpr_cn` objects.

## Usage

``` r
update_bsplines(object, ..., evaluate = TRUE)

update_btensor(object, ..., evaluate = TRUE)
```

## Arguments

- object:

  an object to update.

- ...:

  arguments to update, expected to be `iknots`, `df`, `bknots`, or
  `order`.

- evaluate:

  whether or not to evaluate the updated call.

## Value

If `evaluate = TRUE` then a `cpr_bs` or `cpr_bt` object is returned,
else, an unevaluated call is returned.

## See also

[`update`](https://rdrr.io/r/stats/update.html),
[`bsplines`](http://www.peteredewitt.com/cpr/reference/bsplines.md),
[`btensor`](http://www.peteredewitt.com/cpr/reference/btensor.md)

## Examples

``` r
################################################################################
##                          Updating a cpr_bs object                          ##
# construct a B-spline basis
bmat <- bsplines(runif(10, 1, 10), df = 5, order = 3, bknots = c(1, 10))

# look at the structure of the basis
str(bmat)
#>  'cpr_bs' num [1:10, 1:5] 0.1994 0 0 0 0.0132 ...
#>  - attr(*, "order")= num 3
#>  - attr(*, "df")= num 5
#>  - attr(*, "iknots")= num [1:2] 3.57 5.23
#>  - attr(*, "bknots")= num [1:2] 1 10
#>  - attr(*, "xi")= num [1:8] 1 1 1 3.57 5.23 ...
#>  - attr(*, "xi_star")= num [1:5] 1 2.28 4.4 7.62 10
#>  - attr(*, "call")= language bsplines(x = runif(10, 1, 10), df = 5, bknots = c(1, 10), order = 3)
#>  - attr(*, "environment")=<environment: 0x55586a84e0e8> 

# change the order
str(update_bsplines(bmat, order = 4))
#>  'cpr_bs' num [1:10, 1:5] 0.00511 0 0 0.04359 0 ...
#>  - attr(*, "order")= num 4
#>  - attr(*, "df")= num 5
#>  - attr(*, "iknots")= num 5.91
#>  - attr(*, "bknots")= num [1:2] 1 10
#>  - attr(*, "xi")= num [1:9] 1 1 1 1 5.91 ...
#>  - attr(*, "xi_star")= num [1:5] 1 2.64 5.64 8.64 10
#>  - attr(*, "call")= language bsplines(x = runif(10, 1, 10), df = 5, bknots = c(1, 10), order = 4)
#>  - attr(*, "environment")=<environment: 0x55586a84e0e8> 

# change the order and the degrees of freedom
str(update_bsplines(bmat, df = 12, order = 4))
#>  'cpr_bs' num [1:10, 1:12] 0 0 0 0 0.263 ...
#>  - attr(*, "order")= num 4
#>  - attr(*, "df")= num 12
#>  - attr(*, "iknots")= num [1:8] 2.21 2.56 2.85 3.27 3.68 ...
#>  - attr(*, "bknots")= num [1:2] 1 10
#>  - attr(*, "xi")= num [1:16] 1 1 1 1 2.21 ...
#>  - attr(*, "xi_star")= num [1:12] 1 1.4 1.92 2.54 2.89 ...
#>  - attr(*, "call")= language bsplines(x = runif(10, 1, 10), df = 12, bknots = c(1, 10), order = 4)
#>  - attr(*, "environment")=<environment: 0x55586a84e0e8> 

################################################################################
##                          Updating a cpr_bt object                          ##
# construct a tensor product
tpmat <- btensor(list(x1 = seq(0, 1, length = 10), x2 = seq(0, 1, length = 10)),
                 df = list(4, 5))
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
tpmat
#> Tensor Product Matrix dims: [10 x 20]
#> 

# update the degrees of freedom
update_btensor(tpmat, df = list(6, 7))
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
#> Tensor Product Matrix dims: [10 x 42]
#> 

################################################################################
##      Updating bsplines or btensor on the right and side of a formula       ##

f1 <- y ~ bsplines(x, df = 14) + var1 + var2
f2 <- y ~ btensor(x = list(x1, x2), df = list(50, 31), order = list(3, 5))  + var1 + var2

update_bsplines(f1, df = 13, order = 5)
#> y ~ bsplines(x, df = 13, order = 5) + var1 + var2
#> <environment: 0x55586a84e0e8>
update_btensor(f2, df = list(13, 24), order = list(3, 8))
#> y ~ btensor(x = list(x1, x2), df = list(13, 24), order = list(3, 
#>     8)) + var1 + var2
#> <environment: 0x55586a84e0e8>

################################################################################
##                          Updating a cpr_cp object                          ##
data(spdg, package = "cpr")
init_cp <- cp(pdg ~ bsplines(day, df = 30) + age + ttm, data = spdg)
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
updt_cp <- update_bsplines(init_cp, df = 5)
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)

################################################################################
##                          Updating a cpr_cn object                          ##
init_cn <- cn(pdg ~ btensor(list(day, age), df = list(30, 4)) + ttm, data = spdg)
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
updt_cn <- update_btensor(init_cn, df = list(30, 2), order = list(3, 2))
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
```
