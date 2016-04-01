#' Influence Weights
#'
#' Determine the influence weight of each iternal knot on each marginal of a
#' tensor product.
#'
#' @param tp a cpr_tensor object
#' @param theta corresponding ordinates for \code{x}
#'
#' @return
#' A list with each element a numeric vector of influence weights for the
#' internal knots on a marginal
#'
#'
#' @examples
#' ## NOT RUN
#'
#' data("diamonds", package = "ggplot2")
#' sub_diamonds <- diamonds[1:11000, ]
#' 
#' tp_diamonds <- with(sub_diamonds, cpr::tensor(list(carat, depth, table), iknots = list(c(0.5, 1.5), numeric(0), numeric(0))))
#' 
#' fit <- lm(price ~ tp_diamonds + 0, data = sub_diamonds)
#' theta_diamonds <- unname(coef(fit))
#' 
#' influence_weights(tp_diamonds, theta_diamonds)
#'
#'
#' @export 
influence_weights <- function(tp, theta) { 

  # gather the degrees of freedom for each of the marginal B-splines.
  cols <- mapply(function(ik, k) { length(ik) + k },
                 ik = attr(tp, "iknots"),
                 k  = attr(tp, "orders"), 
                 SIMPLIFY = FALSE) 

  # create a list of matrices for the coefficients need for each marginal
  # evaluation
  marginal_thetas <- 
    lapply(seq_along(attr(tp, "x")), function(m) {
           apply(array(theta, dim = do.call(c, cols)), m, function(x) x)
                   })

  # create a list of the subparts of the tp needed for each marginal assessment.
  col_idx <- 
    lapply(seq_along(attr(tp, "x")), function(m) {
           apply(array(seq(1, ncol(tp), by = 1), dim = do.call(c, cols)), m, function(x) x)
                   })

  # okay, so now we need to get the tensors for each marginal assessment
  marginal_tensors <- 
    lapply(seq_along(attr(tp, "x")), 
           function(x) seq(1, length(attr(tp, "x")))[-x])
  marginal_tensors <- 
    lapply(marginal_tensors, function(i) { 
           if (length(i) == 0) { 
             diag(cols[[1]])
           } else {
             cpr::tensor(x      = attr(tp, "x")[i], 
                         iknots = attr(tp, "iknots")[i],
                         bknots = attr(tp, "bknots")[i],
                         orders = attr(tp, "orders")[i])
           }
           })

    # now find the influence weight of each internal knot on each marginal 
    xis <- mapply(function(b, i, k) { sort(c(rep(b, k), i)) },
                  b = attr(tp, "bknots"),
                  i = attr(tp, "iknots"),
                  k = attr(tp, "orders"), 
                  SIMPLIFY = FALSE)

    thetas <- mapply(function(x, y) {t(x %*% y)}, x = marginal_tensors, y = marginal_thetas, SIMPLIFY = FALSE)

    influence_weights <- 
      mapply(function(xi, th, k) { 
             if (length(xi) > 2 * k) { 
               apply(apply(th, 2, function(tt, xx, k) {cpr::weigh_iknots(xx, tt, k)}, xx = xi, k = k),
                     1, max)
             } else { 
               numeric(0)
             }
                    }, 
                    xi = xis,
                    th = thetas,
                    k  = attr(tp, "orders"),
                    SIMPLIFY = FALSE)

    influence_weights
}

################################################################################
# end of file
################################################################################
