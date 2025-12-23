# Build Tensor

Tensor products of Matrices.

## Usage

``` r
build_tensor(x = NULL, y = NULL, ...)
```

## Arguments

- x:

  a matrix

- y:

  a matrix

- ...:

  additional numeric matrices to build the tensor product

## Value

a matrix

A matrix

## See also

[`vignette("cnr", package = "cpr")`](http://www.peteredewitt.com/cpr/articles/cnr.md)
for details on tensor products.

## Examples

``` r
A <- matrix(1:4, nrow = 10, ncol = 20)
B <- matrix(1:6, nrow = 10, ncol = 6)

# Two ways of building the same tensor product
tensor1 <- build_tensor(A, B)
tensor2 <- do.call(build_tensor, list(A, B))
all.equal(tensor1, tensor2)
#> [1] TRUE

# a three matrix tensor product
tensor3 <- build_tensor(A, B, B)
str(tensor3)
#>  num [1:10, 1:720] 1 8 27 64 25 72 3 16 9 32 ...
```
