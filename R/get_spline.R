#' Get the Control Polygon and the Spline Function
#'
#' Generate \code{data.frame}s for interpolating and plotting a spline
#' function, given a \code{cpr_cp} or \code{cpr_cn} object.
#'
#' A control polygon, \code{cpr\_cp} object, has a spline function f(x).
#' \code{get_spline} returns a list of two \code{data.frame}.  The \code{cp}
#' element is a \code{data.frame} with the (x, y) coordinates control points and
#' the \code{spline} element is a \code{data.frame} with \code{n} rows for
#' interpolating f(x).
#'
#' For a control net, \code{cpr\_cn} object, the return is the same as for a
#' \code{cpr\_cp} object, but conceptually different.  Where a \code{cpr\_cp}
#' objects have a uni-variable spline function, \code{cpr\_cn} have
#' multi-variable spline surfaces.  \code{get_spline} returns a "slice" of the
#' higher dimensional object.  For example, consider a three-dimensional control
#' net defined on the unit cube with marginals \code{x1}, \code{x2}, and
#' \code{x3}.  The implied spline surface is the function f(x1, x2, x3).
#' \code{get_spline(x, margin = 2, at = list(0.2, NA, 0.5))} would
#' return the control polygon and spline surface for f(0.2, x, 0.5).
#'
#' See \code{\link{get_surface}} for taking a two-dimensional slice of a
#' three-plus dimensional control net, or, for generating a useful data set for
#' plotting the surface of a two-dimensional control net.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param x a \code{cpr_cp} or \code{cpr_cn} object.
#' @param margin an integer identifying the marginal of the control net to slice
#' along.  Only used when working \code{x} is a \code{cpr_cn} object.
#' @param at point value for marginals not defined in the \code{margin}.  Only
#' used when \code{x} is a \code{cpr_cn} object.  Expected input is a list of
#' length \code{length(attr(x, "bspline_list"))}.  Entries for elements
#' \code{marginal} are ignored.  If omitted, the midpoint between the boundary
#' knots for each marginal is used.
#' @param n the length of sequence to use for interpolating the spline function.
#'
#' @seealso \code{\link{get_surface}}
#'
#' @examples
#' data(spdg, package = "cpr")
#' 
#' ## Extract the control polygon and spline for plotting.  We'll use base R
#' ## graphics for this example.
#' a_cp <- cp(pdg ~ bsplines(day, df = 10), data = spdg)
#' 
#' cp_and_spline <- get_spline(a_cp)
#' plot(cp_and_spline$cp, type = "b")
#' points(cp_and_spline$spline, type = "l")
#' grid()
#' 
#' # compare to the cpr:::plot.cpr_cp method
#' plot(a_cp, show_spline = TRUE)
#' 
#' @export
get_spline <- function(x, margin = 1, at, n = 100) {
  UseMethod("get_spline")
}

#' @export
get_spline.cpr_cp <- function(x, margin = 1, at, n = 100) {
  xvec <- seq(min(x$bknots), max(x$bknots), length = n)
  bmat <- bsplines(xvec, iknots = x$iknots, bknots = x$bknots, order = x$order)
  out <- list(cp     = x$cp,
              spline = dplyr::as_data_frame(data.frame(x = xvec, y = as.numeric(bmat %*% x[["cp"]][["theta"]]))))
  out
}

#' @export
get_spline.cpr_cn <- function(x, margin = 1, at, n = 100) {

  if (length(margin) > 1) {
    stop("use get_surface when length(margin) > 1.", call. = FALSE)
  }

  if (missing(at)) { 
    at <- lapply(lapply(x$bspline_list, attr, which = "bknots"), mean)
  } 

  dfs    <- sapply(x$bspline_list, ncol)
  bknots <- lapply(x$bspline_list, attr, which = "bknots")
  iknots <- lapply(x$bspline_list, attr, which = "iknots")
  orders <- lapply(x$bspline_list, attr, which = "order")

  mbs <- mapply(bsplines, x = at, iknots = iknots, bknots = bknots, order = orders, SIMPLIFY = FALSE)
  tensor <- build_tensor(mbs[-margin])
  thetas <- apply(array(x$cn$theta, dim = dfs), margin, function(x) x)
  marginal_cp <- cp(x$bspline_list[[margin]], t(tensor %*% thetas))
  get_spline.cpr_cp(marginal_cp) 
}

#' Get Two-Dimensional Control Net and Surface from n-dimensional Control Nets
#'
#' 
#' @param x a \code{cpr_cn} object
#' @param margin an integer identifying the marginal of the control net to slice
#' along.  Only used when working \code{x} is a \code{cpr_cn} object.
#' @param at point value for marginals not defined in the \code{margin}.  Only
#' used when \code{x} is a \code{cpr_cn} object.  Expected input is a list of
#' length \code{length(attr(x, "bspline_list"))}.  Entries for elements
#' \code{marginal} are ignored.  If omitted, the midpoint between the boundary
#' knots for each marginal is used.
#' @param n the length of sequence to use for interpolating the spline function.
#'
#' @seealso \code{\link{get_spline}}
#'
#' @examples
#' ## Extract the control net and surface from a cpr_cn object.
#' a_cn <- cn(pdg ~ btensor(list(day, age), df = list(15, 3), order = list(3, 2)),
#'            data = spdg)
#' 
#' cn_and_surface <- get_surface(a_cn)
#' str(cn_and_surface, max.level = 2)
#' 
#' par(mfrow = c(1, 2))
#' with(cn_and_surface$cn, 
#'      plot3D::persp3D(unique(Var1), 
#'                      unique(Var2), 
#'                      matrix(z,
#'                             nrow = dplyr::n_distinct(Var1), 
#'                             ncol = dplyr::n_distinct(Var2)),
#'                      main = "Control Net")
#'      )
#' with(cn_and_surface$surface, 
#'      plot3D::persp3D(unique(Var1), 
#'                      unique(Var2), 
#'                      matrix(z,
#'                             nrow = dplyr::n_distinct(Var1), 
#'                             ncol = dplyr::n_distinct(Var2)),
#'                      main = "Surface")
#'      )
#'
#' 
#' @export
get_surface <- function(x, margin = 1:2, at, n = 100) {
  UseMethod("get_surface")
} 

#' @export
get_surface.cpr_cn <- function(x, margin = 1:2, at, n = 100) {
  if (missing(at)) { 
    at <- lapply(lapply(x$bspline_list, attr, which = "bknots"), mean)
  } 
  dfs    <- sapply(x$bspline_list, ncol)
  bknots <- lapply(x$bspline_list, attr, which = "bknots")
  iknots <- lapply(x$bspline_list, attr, which = "iknots")
  orders <- lapply(x$bspline_list, attr, which = "order")

  # The control net
  xvecs <- lapply(x$bspline_list, attr, which = "xi_star")
  xvecs[-margin] <- at[-margin] 
  net <- do.call(expand.grid, xvecs) 
  tensors <- Map(btensor,
                 x = split(net, row(net)[, 1]), 
                 MoreArgs = list(iknots = iknots,
                                 bknots = bknots,
                                 order = orders)) 
  net$z <- do.call(c, Map(`%*%`, x = tensors,
                          MoreArgs = list(y = x$cn$theta))) 

  # the surface
  xvecs <- lapply(bknots, function(x) seq(x[1], x[2], length = 100))
  xvecs[-margin] <- at[-margin] 
  surface <- do.call(expand.grid, xvecs) 
  tensors <- Map(btensor,
                 x = split(surface, row(surface)[, 1]), 
                 MoreArgs = list(iknots = iknots,
                                 bknots = bknots,
                                 order = orders)) 
  surface$z <- do.call(c, Map(`%*%`, x = tensors,
                              MoreArgs = list(y = x$cn$theta)))

  list(cn = net, surface = surface)
} 
