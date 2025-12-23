# Generate Control Polygon Formula and Data

Construct a `data.frame` and `formula` to be passed to the regression
modeling tool to generate a control polygon.

## Usage

``` r
generate_cp_formula_data(f, data, formula_only = FALSE, envir = parent.frame())
```

## Arguments

- f:

  a formula

- data:

  the data set containing the variables in the formula

- formula_only:

  if TRUE then only generate the formula, when FALSE, then generate and
  assign the data set too.

- envir:

  the environment the generated formula and data set will be assigned
  too.

## Value

TRUE, invisibly. The return isn't needed as the assignment happens
within the call.

## Details

This function is expected to be called from within the `cp` function and
is not expected to be called by the end user directly.

`generate_cp_data` exists because of the need to build what could be
considered a varying means model. `y ~ bsplines(x1) + x2` will generate
a rank deficient model matrix—the rows of the bspline basis matrix sum
to one with is perfectly collinear with the implicit intercept term.
Specifying a formula `y ~ bsplines(x1) + x2 - 1` would work if `x2` is a
continuous variable. If, however, `x2` is a factor, or coerced to a
factor, then the model matrix will again be rank deficient as a column
for all levels of the factor will be generated. We need to replace the
intercept column of the model matrix with the bspline. This also needs
to be done for a variety of possible model calls,
[`lm`](https://rdrr.io/r/stats/lm.html),
[`lmer`](https://rdrr.io/pkg/lme4/man/lmer.html), etc.

By returning an explicit `formula` and `data.frame` for use in the fit,
we hope to reduce memory use and increase the speed of the cpr method.

We need to know the `method` and `method.args` to build the data set.
For example, for a
[`geeglm`](https://rdrr.io/pkg/geepack/man/geeglm.html) the `id`
variable is needed in the data set and is part of the `method.args` not
the `formula`.

## Examples

``` r
 data <-
   data.frame(
                x1 = runif(20)
              , x2 = runif(20)
              , x3 = runif(20)
              , xf = factor(rep(c("l1","l2","l3","l4"), each = 5))
              , xc = rep(c("c1","c2","c3","c4", "c5"), each = 4)
              , pid = gl(n = 2, k = 10)
              , pid2 = rep(1:2, each = 10)
   )

 f <- ~ bsplines(x1, bknots = c(0,1)) + x2 + xf + xc + (x3 | pid2)

 cpr:::generate_cp_formula_data(f, data)

 stopifnot(isTRUE(
   all.equal(
             f_for_use
             ,
             . ~ bsplines(x1, bknots = c(0, 1)) + x2 + (x3 | pid2) + xfl2 +
                 xfl3 + xfl4 + xcc2 + xcc3 + xcc4 + xcc5 - 1
             )
 ))

 stopifnot(isTRUE(identical(
   names(data_for_use)
   ,
   c("x1", "x2", "x3", "pid", "pid2", "xfl2", "xfl3", "xfl4"
     , "xcc2" , "xcc3", "xcc4", "xcc5")
 )))
```
