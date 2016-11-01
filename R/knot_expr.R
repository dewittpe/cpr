
knot_expr <- function(x) {
  UseMethod("knot_expr")
}

knot_expr.cpr_cp <- function(x) {
  expr <- list(bquote(group('{', xi[j], '}')[j == 1]^{.(x$order)})) 
  if (length(x$xi) > 2 * x$order) { 
    for(i in seq(x$order + 1, length(x$xi) - x$order, by = 1)) { 
      expr <- c(expr, bquote(xi[.(i)]))
    }
  } 
  expr <- c(expr, bquote(group('{', xi[j], '}')[j == .(length(x$xi) - x$order + 1L)]^{.(length(x$xi))})) 
  expr
}

knot_expr.cpr_bs <- function(x) {
  expr <- list(bquote(group('{', xi[j], '}')[j == 1]^{.(attr(x, "order"))})) 
  if (length(attr(x, "xi")) > 2 * attr(x, "order")) { 
    for(i in seq(attr(x, "order") + 1, length(attr(x, "xi")) - attr(x, "order"), by = 1)) { 
      expr <- c(expr, bquote(xi[.(i)]))
    }
  } 
  expr <- c(expr, bquote(group('{', xi[j], '}')[j == .(length(attr(x, "xi")) - attr(x, "order") + 1L)]^{.(length(attr(x, "xi")))}))
  expr
}
