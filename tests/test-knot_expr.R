library(cpr)

################################################################################
# vertify knot_expr is an S3 method and not exported
stopifnot(!grepl("knot_expr", ls("package:cpr")))

cpr_namespace <- ls( getNamespace("cpr"), all.names = TRUE)
stopifnot(sum(cpr_namespace == "knot_expr") == 1L)
stopifnot(sum(cpr_namespace == "knot_expr.cpr_bs") == 1L)
stopifnot(sum(cpr_namespace == "knot_expr.cpr_cp") == 1L)
stopifnot(sum(cpr_namespace == "generate_knot_expression") == 1L) # called by the S3 methods
stopifnot(sum(grepl("^knot_expr", cpr_namespace)) == 3L)

################################################################################
# Testing cpr_bs method
e <- new.env()
with(e, {

  bmat <- bsplines(mtcars$hp, df = 8)
  ke <- cpr:::knot_expr(bmat, digits = 1)
  stopifnot(identical(ke$breaks, sort(c(attr(bmat, "bknots"), attr(bmat, "iknots")))))
  stopifnot(inherits(ke$xi_expr, "list"))
  stopifnot(identical(length(ke$xi_expr), 2L + length(attr(bmat, "iknots"))))

  stopifnot(identical(ke$xi_expr[[1]], bquote(group("{", xi[j], "}")[j == 1]^4)))
  stopifnot(identical(ke$xi_expr[[2]], bquote(xi[5])))
  stopifnot(identical(ke$xi_expr[[3]], bquote(xi[6])))
  stopifnot(identical(ke$xi_expr[[4]], bquote(xi[7])))
  stopifnot(identical(ke$xi_expr[[5]], bquote(xi[8])))
  stopifnot(identical(ke$xi_expr[[6]], bquote(group("{", xi[j], "}")[j == 9]^12)))

})

################################################################################
# Testing cpr_cp method
e <- new.env()
with(e, {

  bmat <- bsplines(mtcars$hp, df = 8)
  theta <- rnorm(4)
  acp <- cp(bmat, theta)
  ke <- cpr:::knot_expr(acp, digits = 1)
  stopifnot(identical(ke$breaks, sort(c(attr(bmat, "bknots"), attr(bmat, "iknots")))))
  stopifnot(inherits(ke$xi_expr, "list"))
  stopifnot(identical(length(ke$xi_expr), 2L + length(attr(bmat, "iknots"))))

  stopifnot(identical(ke$xi_expr[[1]], bquote(group("{", xi[j], "}")[j == 1]^4)))
  stopifnot(identical(ke$xi_expr[[2]], bquote(xi[5])))
  stopifnot(identical(ke$xi_expr[[3]], bquote(xi[6])))
  stopifnot(identical(ke$xi_expr[[4]], bquote(xi[7])))
  stopifnot(identical(ke$xi_expr[[5]], bquote(xi[8])))
  stopifnot(identical(ke$xi_expr[[6]], bquote(group("{", xi[j], "}")[j == 9]^12)))

})

################################################################################
##                                End of File                                 ##
################################################################################
