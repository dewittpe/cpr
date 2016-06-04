#' Influence Weights
#'
#' Determine the influence weight of each iternal knot on each marginal of a
#' tensor product.
#'
#' @param x a \code{cpr_cp} or \code{cpr_tensor} object
#' @param p the order of the norm, default \code{p = 2}.
#'
#' @return
#' A data_frame with two elements, the internal knots (iknots) and the weights.
#'
#' @examples
#' 
#' xvec    <- seq(0, 6, length = 20)
#' iknots  <- c(1, 1.5, 2.3, 4, 4.5)
#' bmat    <- cpr::bsplines(x = xvec, iknots = iknots, bknots = c(0, 6), order = 4)
#' theta   <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
#' 
#' example_cp <- cp(bmat, theta)
#' influence_weights(example_cp) 
#'
#' @export 
influence_weights <- function(x, p = 2) { 
  UseMethod("influence_weights")
}

#' @export 
influence_weights.cpr_cp <- function(x, p = 2) {
  if (length(x$iknots) > 0) { 
    iw <- .Call('cpr_weigh_iknots', PACKAGE = 'cpr', x$xi, x$cp$theta, x$order, p) 
    # dplyr::data_frame(xi = x$xi, w = c(rep(NA, x$order), iw, rep(NA, x$order))) 
    dplyr::data_frame(iknots = x$iknots, w = c(iw))
  } else { 
    dplyr::data_frame(iknots = numeric(0), w = numeric(0))
  }

}

#' @export 
influence_weights.cpr_tensor <- function(x, p= 2) { 

  stop("influcence_weights.cpr_tensor requires a rewrite, do not use.")

#  # gather the degrees of freedom for each of the marginal B-splines.
#  cols <- mapply(function(ik, k) { length(ik) + k },
#                 ik = attr(x, "iknots"),
#                 k  = attr(x, "orders"), 
#                 SIMPLIFY = FALSE) 
#
#  # create a list of matrices for the coefficients need for each marginal
#  # evaluation
#  marginal_thetas <- 
#    lapply(seq_along(attr(x, "x")), function(m) {
#           apply(array(theta, dim = do.call(c, cols)), m, function(x) x)
#                   })
#
#  # create a list of the subparts of the x needed for each marginal assessment.
#  col_idx <- 
#    lapply(seq_along(attr(x, "x")), function(m) {
#           apply(array(seq(1, ncol(x), by = 1), dim = do.call(c, cols)), m, function(x) x)
#                   })
#
#  # okay, so now we need to get the tensors for each marginal assessment
#  marginal_tensors <- 
#    lapply(seq_along(attr(x, "x")), 
#           function(x) seq(1, length(attr(x, "x")))[-x])
#  marginal_tensors <- 
#    lapply(marginal_tensors, function(i) { 
#           if (length(i) == 0) { 
#             diag(cols[[1]])
#           } else {
#             cpr::tensor(x      = attr(x, "x")[i], 
#                         iknots = attr(x, "iknots")[i],
#                         bknots = attr(x, "bknots")[i],
#                         orders = attr(x, "orders")[i])
#           }
#           })
#
#    # now find the influence weight of each internal knot on each marginal 
#    xis <- mapply(function(b, i, k) { sort(c(rep(b, k), i)) },
#                  b = attr(x, "bknots"),
#                  i = attr(x, "iknots"),
#                  k = attr(x, "orders"), 
#                  SIMPLIFY = FALSE)
#
#    thetas <- mapply(function(x, y) {t(x %*% y)}, x = marginal_tensors, y = marginal_thetas, SIMPLIFY = FALSE)
#
#    influence_weights <- 
#      mapply(function(xi, th, k) { 
#             if (length(xi) > 2 * k) { 
#               apply(apply(th, 2, function(tt, xx, k) {cpr::weigh_iknots(xx, tt, k)}, xx = xi, k = k),
#                     1, max)
#             } else { 
#               numeric(0)
#             }
#                    }, 
#                    xi = xis,
#                    th = thetas,
#                    k  = attr(x, "orders"),
#                    SIMPLIFY = FALSE)
#
#    influence_weights
}

################################################################################
# end of file
################################################################################
