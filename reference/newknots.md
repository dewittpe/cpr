# New Knots for CPs and CNs in CPR and CNR

Non-exported function, `newknots` are used in the
[`cpr`](http://www.peteredewitt.com/cpr/reference/cpr.md) and
[`cnr`](http://www.peteredewitt.com/cpr/reference/cnr.md) calls. Used to
create a new control polygon or control net from with different internal
knots.

## Usage

``` r
newknots(form, nk)
```

## Arguments

- form:

  a `formula`

- nk:

  numeric vector, or a list of numeric vectors, to be used in a
  [`bsplines`](http://www.peteredewitt.com/cpr/reference/bsplines.md) or
  [`btensor`](http://www.peteredewitt.com/cpr/reference/btensor.md)
  call, respectively.

## Value

Expected use is within the `cpr` and `cnr` calls. The return object a
formula to define a control polygon/net with different knots than then
ones found within `form`.

## Details

Think of this function as an analogue to the
[`stats`](https://rdrr.io/r/stats/stats-package.html)`{update}` calls.
Where [`stats`](https://rdrr.io/r/stats/stats-package.html)`{update}`
will modify a `call`, the `newknots` will update just the `iknots`
argument of a `bsplines` or `btensor` call within the `formula` argument
of a [`cp`](http://www.peteredewitt.com/cpr/reference/cp.md) or
[`cn`](http://www.peteredewitt.com/cpr/reference/cn.md) call.

## See also

[`update_bsplines`](http://www.peteredewitt.com/cpr/reference/update_bsplines.md)
for a more generic tool for the end user.

## Examples

``` r
cp0 <- cp(log(pdg) ~ bsplines(day, iknots = c(-.25, 0, 0.25), bknots = c(-1, 1)), data = spdg)

new_knots <- c(-0.85, 0, 0.25, 0.3)
f <- cpr:::newknots(cp0$call$formula, nk = new_knots)
f
#> log(pdg) ~ bsplines(day, iknots = c(-0.85, 0, 0.25, 0.3), bknots = c(-1, 
#>     1))
#> NULL
cp(f, data = spdg)
#>      xi_star        theta
#> 1 -1.0000000 -0.007699847
#> 2 -0.9500000 -0.439608186
#> 3 -0.6166667 -1.086005675
#> 4 -0.2000000 -1.657347591
#> 5  0.1833333  0.570416069
#> 6  0.5166667  1.960042179
#> 7  0.7666667  1.477610529
#> 8  1.0000000  0.088837397
```
