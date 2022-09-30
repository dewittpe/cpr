library(cpr)

# test that knot_expr
bmat <- bsplines(mtcars$hp, df = 8)
ke <- cpr:::knot_expr.cpr_bs(bmat, digits = 1)

stopifnot(all.equal(ke$xi_expr[[4]], bquote(xi[7])))

