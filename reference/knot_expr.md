# Knot Expressions

Non-exported function used to build expressions for the knot sequences
to be labeled well on a plot.

## Usage

``` r
knot_expr(x, digits)
```

## Arguments

- x:

  a `cpr_cp` or `cpr_bs` object

- digits:

  digits to the right of the decimal point to report

## Value

a list

## Examples

``` r
bmat <- bsplines(mtcars$hp, df = 8, bknots = c(50, 350))
ke <- cpr:::knot_expr(bmat, digits = 1)
summary(ke)
#>          Length Class  Mode     
#> breaks   6      -none- numeric  
#> xi_expr  6      -none- list     
#> num_expr 6      -none- character

plot(x = ke$breaks, y = rep(1, length(ke$breaks)), type = "n")
text(
       x = ke$breaks
     , y = rep(1, length(ke$breaks))
     , labels = parse(text = ke$xi_expr)
)

```
