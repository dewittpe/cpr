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
#' cn_and_surface <- get_surface(a_cn, n = 50)
#' str(cn_and_surface, max.level = 2)
#'
#' par(mfrow = c(1, 2))
#' with(cn_and_surface$cn,
#'      plot3D::persp3D(unique(Var1),
#'                      unique(Var2),
#'                      matrix(z,
#'                             nrow = length(unique(Var1)),
#'                             ncol = length(unique(Var2))),
#'                      main = "Control Net")
#'      )
#' with(cn_and_surface$surface,
#'      plot3D::persp3D(unique(Var1),
#'                      unique(Var2),
#'                      matrix(z,
#'                             nrow = length(unique(Var1)),
#'                             ncol = length(unique(Var2))),
#'                      main = "Surface")
#'      )
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

  list(cn      = net[c(margin, ncol(net))],
       surface = surface[c(margin, ncol(net))])
}
