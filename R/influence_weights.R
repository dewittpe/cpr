#' Influence Weights
#'
#' Determine the influence weight of each internal knot on each marginal of a
#' tensor product.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param x a \code{cpr_cp} or \code{cpr_cn} object
#' @param p the order of the norm, default \code{p = 2}.
#' @param n_polycoef for influence weights in tensor products, this parameter
#' set the number of polynomial coefficients to use in each of the marginal
#' calculations.  Ignored for \code{cpr_cp} objects.
#' @param margin The margins to apply CNR to.  Ignored for \code{cpr_cp}
#' objects.
#'
#' @return
#' A data_frame with two elements, the internal knots (iknots) and the weights.
#'
#' @export
influence_weights <- function(x, p = 2, margin = seq_along(x$bspline_list), n_polycoef = 20L) {
  UseMethod("influence_weights")
}

#' @export
influence_weights.cpr_cp <- function(x, p = 2, margin = NULL, n_polycoef = NULL) {
  if (length(x$iknots) > 0) {
    iw <- .Call('_cpr_weigh_iknots', PACKAGE = 'cpr', x$xi, matrix(x$cp$theta, ncol = 1), x$order, p)
    dplyr::data_frame(iknots = x$iknots, w = c(iw))
  } else {
    dplyr::data_frame(iknots = numeric(0), w = numeric(0))
  }

}

#' @export
influence_weights.cpr_cn <- function(x, p = 2, margin = seq_along(x$bspline_list), n_polycoef = 20L) {

  dfs    <- sapply(x$bspline_list, ncol)
  bknots <- lapply(x$bspline_list, attr, which = "bknots")
  iknots <- lapply(x$bspline_list, attr, which = "iknots")
  orders <- lapply(x$bspline_list, attr, which = "order")

  xvecs <-
    mapply(seq,
           from = lapply(bknots, min),
           to   = lapply(bknots, max),
           MoreArgs = list(length = n_polycoef),
           SIMPLIFY = FALSE)

  marginal_bsplines <-
    mapply(bsplines,
           x = xvecs,
           iknots = iknots,
           bknots = bknots,
           order  = orders,
           SIMPLIFY = FALSE)

  marginal_tensors <-
    lapply(seq_along(marginal_bsplines),
           function(idx) {
             build_tensor(marginal_bsplines[-idx])
           })

  marginal_thetas <-
    lapply(seq_along(x$bspline_list),
           function(m) {
             apply(array(x$cn$theta, dim = dfs), m, function(x) x)
           })

  polynomial_coef <-
    mapply(function(xx, yy) {t(xx %*% yy)},
           xx = marginal_tensors,
           yy = marginal_thetas,
           SIMPLIFY = FALSE)

  wghts <- 
    lapply(seq_along(x$bspline_list)[margin],
           function(idx) {
             lapply(split(polynomial_coef[[idx]], col(polynomial_coef[[idx]])),
                    function(tt, bmat) {
                      influence_weights.cpr_cp(cp(bmat, tt), p)
                    },
                    bmat = x$bspline_list[[idx]]) %>%
             dplyr::bind_rows(wghts) %>%
             dplyr::group_by_(wghts, ~ iknots) %>%
             dplyr::summarize_(wghts, ~ max(w))
           })
  out <- lapply(iknots, function(ik) dplyr::data_frame(iknots = ik, `max(w)` = rep(Inf, length(ik))))
  out[seq_along(x$bspline_list) %in% margin] <- wghts
  out
} 
>>>>>>> fix-Rcpp
