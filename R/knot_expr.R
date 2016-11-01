knot_expr <- function(x, digits, show_xi, show_x) {
  UseMethod("knot_expr")
}

knot_expr.cpr_cp <- function(x, digits, show_xi, show_x) { 
  generate_knot_expression(x$xi, x$order, x$b_knots, digits, show_xi, show_x) 
}

knot_expr.cpr_bs <- function(x, digits, show_xi, show_x) { 
  generate_knot_expression(attr(x, "xi"), attr(x, "order"), attr(x, "bknots"), digits, show_xi, show_x) 
}


generate_knot_expression <- function(xi, k, bk, digits, show_xi, show_x) {

  if (show_xi & show_x) { 
    expr <- list(bquote(atop(group('{', xi[j], '}')[j == 1]^{.(k)}, .(formatC(bk[1], digits, format = "f"))))) 
    if (length(xi) > 2 * k) { 
      for(i in seq(k + 1, length(xi) - k, by = 1)) { 
        expr <- c(expr, bquote(atop(xi[.(i)], .(formatC(xi[i], digits, format = "f")))))
      }
    } 
    expr <- c(expr, bquote(atop(group('{', xi[j], '}')[j == .(length(xi) - k + 1L)]^{.(length(xi))}, .(formatC(bk[2], digits, format = "f")))))
  } else if (show_xi & !show_x) { 
    expr <- list(bquote(group('{', xi[j], '}')[j == 1]^{.(k)})) 
    if (length(xi) > 2 * k) { 
      for(i in seq(k + 1, length(xi) - k, by = 1)) { 
        expr <- c(expr, bquote(xi[.(i)]))
      }
    } 
    expr <- c(expr, bquote(group('{', xi[j], '}')[j == .(length(xi) - k + 1L)]^{.(length(xi))})) 
  } else if (!show_xi & show_x) { 
    expr <- list(bquote(.(formatC(bk[1], digits, format = "f"))))
    if (length(xi) > 2 * k) { 
      for(i in seq(k + 1, length(xi) - k, by = 1)) { 
        expr <- c(expr, bquote(.(formatC(xi[i], digits, format = "f"))))
      }
    } 
    expr <- c(expr, bquote(.(formatC(bk[2], digits, format = "f")))) 
  }


  list(breaks = unique(xi),
       labels = do.call(expression, expr))
}
