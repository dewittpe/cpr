# btensor

Tensor products of B-splines.

## Usage

``` r
btensor(x, df = NULL, iknots = NULL, bknots, order)
```

## Arguments

- x:

  a list of variables to build B-spline transforms of. The tensor
  product of these B-splines will be returned.

- df:

  degrees of freedom. A list of the degrees of freedom for each
  marginal.

- iknots:

  a list of internal knots for each x. If omitted, the default is to
  place no internal knots for all x. If specified, the list needs to
  contain the internal knots for all x. If `df` and `iknots` are both
  given, the `df` will take precedence.

- bknots:

  a list of boundary knots for each x. As with the iknots, if omitted
  the default will be to use the range of each x. If specified, the use
  must specify the bknots for each x.

- order:

  a list of the order for each x; defaults to 4L for all x.

## Value

A matrix with a class `cpr_bt`

## Details

The return form this function is the tensor product of the B-splines
transformations for the given variables. Say we have variables X, Y, and
Z to build the tensor product of. The columns of the returned matrix
correspond to the column products of the three B-splines:

x1y1z1 x2y1z1 x3y1z1 x4y1z1 x1y2z1 x2y2z1 ... x4y4z4

for three fourth order B-splines with no internal knots. The columns of
X cycle the quickest, followed by Y, and then Z. This would be the same
result as
` model.matrix( ~ bsplines(X) : bsplines(Y) : bsplines(Z) + 0) `.

See
[`vignette(topic = "cnr", package = "cpr")`](http://www.peteredewitt.com/cpr/articles/cnr.md)
for more details.

## See also

[`bsplines`](http://www.peteredewitt.com/cpr/reference/bsplines.md),
[`vignette(topic = "cnr", package = "cpr")`](http://www.peteredewitt.com/cpr/articles/cnr.md)

## Examples

``` r
tp <- with(mtcars,
           btensor(x = list(d = disp, h = hp, m = mpg),
                   iknots = list(numeric(0), c(100, 150), numeric(0)))
           )
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
#> Warning: At least one x value >= max(bknots)
tp
#> Tensor Product Matrix dims: [32 x 96]
#> 
```
