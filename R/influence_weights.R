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
#' @export 
influence_weights <- function(x, p = 2) { 
  UseMethod("influence_weights")
}

#' @export 
influence_weights.cpr_cp <- function(x, p = 2) {
  if (length(x$iknots) > 0) { 
    iw <- .Call('cpr_weigh_iknots', PACKAGE = 'cpr', x$xi, matrix(x$cp$theta, ncol = 1), x$order, p) 
    dplyr::data_frame(iknots = x$iknots, w = c(iw))
  } else { 
    dplyr::data_frame(iknots = numeric(0), w = numeric(0))
  }

}

#' @export 
influence_weights.cpr_cn <- function(x, p = 2) { 

  dfs <- sapply(x$bspline_list, ncol)

  marginal_thetas <- 
    lapply(seq_along(x$bspline_list), 
           function(m) {
             apply(array(x$cn$theta, dim = dfs), m, function(x) x)
           })

  marginal_tensors <- 
    lapply(seq_along(x$bspline_list), 
           function(idx) {
             build_tensor(x$bspline_list[-idx])
           })

  polynomial_coef <- 
    mapply(function(xx, yy) {t(xx %*% yy)},
           xx = marginal_tensors, 
           yy = marginal_thetas, 
           SIMPLIFY = FALSE)

  parallel::mclapply(seq_along(x$bspline_list),
                     function(idx) { 

                       wghts <- 
                         lapply(split(polynomial_coef[[idx]], col(polynomial_coef[[idx]])),
                                function(tt, bmat) { 
                                  influence_weights.cpr_cp(cp(bmat, tt))
                                },
                                bmat = x$bspline_list[[idx]])
                       wghts <- dplyr::bind_rows(wghts)
                       wghts <- dplyr::group_by_(wghts, ~ iknots)
                       wghts <- dplyr::summarize_(wghts, ~ max(w))
                       wghts 
                     }) 
}

################################################################################
# end of file
################################################################################
