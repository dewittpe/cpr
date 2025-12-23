# Vertical Difference between two Control Polygons

Vertical Difference between two Control Polygons

## Usage

``` r
cp_diff(cp1, cp2)
```

## Arguments

- cp1:

  a `cpr_cp` object

- cp2:

  a `cpr_cp` object

## Value

the vertical distance between the control vertices of cp1 to the control
polygon cp2.

## See also

[`cp`](http://www.peteredewitt.com/cpr/reference/cp.md),
[`cp_value`](http://www.peteredewitt.com/cpr/reference/cp_value.md)

## Examples

``` r
xvec <- runif(n = 500, min = 0, max = 6)

# Define the basis matrix
bmat1 <- bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
bmat2 <- bsplines(x = xvec, bknots = c(0, 6))

# Define the control vertices ordinates
theta1 <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
theta2 <- c(1, 3.4, -2, 1.7)

# build the two control polygons
cp1 <- cp(bmat1, theta1)
cp2 <- cp(bmat2, theta2)

cp_diff(cp1, cp2)
#> [1]  0.0000000  1.4000000 -1.5000000 -1.2800000 -1.9200000 -0.4200000  0.2416667
#> [8] -1.2250000  0.2000000

df <- data.frame(x = cp1$cp$xi_star,
                 y = cp1$cp$theta,
                 yend = cp1$cp$theta + cp_diff(cp1, cp2))


plot(cp1, cp2) +
ggplot2::geom_segment(data = df
  , mapping = ggplot2::aes(x = x, xend = x, y = y, yend = yend)
  , color = "red"
  , inherit.aes = FALSE)
#> Warning: Removed 21 rows containing missing values or values outside the scale range
#> (`geom_rug()`).

```
