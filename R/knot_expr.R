#' Knot Expressions
#'
#' Non-exported function used to build expressions for the knot sequences to be
#' labeled well on a plot.
#'
#' @param x a \code{cpr_cp} or \code{cpr_bs} object
#' @param digits digits to the right of the decimal point to report
#'
knot_expr <- function(x, digits) {
  UseMethod("knot_expr")
}

knot_expr.cpr_cp <- function(x, digits) {
  generate_knot_expression(x$xi, digits)
}

knot_expr.cpr_bs <- function(x, digits) {
  generate_knot_expression(attr(x, "xi"), digits)
}


generate_knot_expression <- function(xi, digits) {

  xi_tab <- table(xi)

  # Index
  j <- seq_along(xi)
  j <- Map(function(xt, idx) { seq(1, xt[idx], by = 1) },
           idx = seq_along(xi_tab),
           MoreArgs = list(xt = as.numeric(xi_tab)))

  for(i in seq_along(j)[-1]) {
    j[[i]] <- max(j[[i - 1]]) + j[[i]]
  }

  xi_expr <- lapply(j,
                    function(x) {
                      if (length(x) > 1) {
                        bquote(group("{", xi[j], "}")[j == .(min(x))]^{.(max(x))})
                      } else {
                        bquote(xi[.(x)])
                      }
                    })

  list(breaks = unique(xi),
       xi_expr = xi_expr,
       num_expr =  formatC(unique(xi), digits, format = "f"))

}
