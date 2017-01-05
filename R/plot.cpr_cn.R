#' Plotting Control Nets
#'
#' One- and Two-dimensional plots of control nets.
#'
#' @method plot cpr_cn
#' @export
#' @param x a \code{cpr_cn} object
#' @param \ldots ignored
#' @param margin an integer vector or length 1 or 2.  For length 1, the marginal
#' control polygon is plotted via a call to \code{\link{plot.cpr_cp}}.  For
#' length 2, the (marginal) tensor surface is plotted.  See details.
#' @param at point value for marginals not defined in the \code{margin}.  See
#' details.
#' @param show_net logical, show the control net
#' @param show_surface logical, show the tensor product surface
#' @param xlim,ylim,zlim x-, y- and z-limits.  If present, the plot is clipped to this region.
#' @param xlab,ylab,zlab titles for the axes.  N.B. These must be character
#' strings; expressions are not accepted.  Numbers will be coerced to character
#' strings.
#' @param aspect aspect ratio of the graphic
#' @param net_args \code{\link[rgl]{rgl.material}} and other arguments passed to
#' \code{\link[rgl]{persp3d}} to modify the appearance of the control net.
#' Defaults to \code{front = "lines", back = "lines"}.
#' @param surface_args \code{\link[rgl]{rgl.material}} and other arguments
#' passed to \code{\link[rgl]{persp3d}} to modify the appearance of the surface.
#' Defaults to \code{front = "lines", back = "lines"}.  
#' @param cp_args arguments passed to \code{\link{plot.cpr_cp}}, only used if
#' \code{length(margin) == 1L}.
#'
#' @return
#' A list with the data for plotting the surface and the net.
#' The return is invisible, i.e., does not print to the screen.
#'
#' @seealso \code{\link{plot.cpr_cp}}, \code{\link[rgl]{persp3d}},
#' \code{\link[rgl]{rgl.material}}
#'
plot.cpr_cn <- function(x, ..., margin = 1:2, at,
                        show_net = TRUE, show_surface = FALSE,
                        xlim, ylim, zlim,
                        xlab = "", ylab = "", zlab = "", 
                        aspect,
                        net_args = list(col = "black"),
                        surface_args = list(col = "grey20"),
                        cp_args = list(show_cp = TRUE, show_spline = TRUE, show_xi = FALSE)) { 
  if (missing(at)) { 
    at <- lapply(lapply(x$bspline_list, attr, which = "bknots"), mean)
  } 
  
  if (!is.list(at) | length(at) != length(x$bspline_list)) {
    stop("the `at` argument needs to be a list of the same length as `x$bspline_list`.")
  } 

  net_args$xlab <- surface_args$xlab <- xlab 
  net_args$ylab <- surface_args$ylab <- ylab 
  net_args$zlab <- surface_args$zlab <- zlab

  if (!missing(xlim)) {
    net_args$xlim <- surface_args$xlim <- xlim
  }

  if (!missing(ylim)) {
    net_args$ylim <- surface_args$ylim <- ylim
  }
   
  if (!missing(zlim)) {
    net_args$zlim <- surface_args$zlim <- zlim
  }

  if (!missing(aspect)) {
    net_args$aspect <- surface_args$aspect <- aspect
  }

  if (is.null(surface_args$front)) {
    surface_args$front <- 'fill'
  }

  if (is.null(surface_args$back)) {
    surface_args$back <- 'lines'
  }

  if (is.null(net_args$front)) {
    net_args$front <- 'lines'
  }

  if (is.null(net_args$back)) {
    net_args$back <- 'lines'
  }

  dfs    <- sapply(x$bspline_list, ncol)
  bknots <- lapply(x$bspline_list, attr, which = "bknots")
  iknots <- lapply(x$bspline_list, attr, which = "iknots")
  orders <- lapply(x$bspline_list, attr, which = "order")

  if (length(margin) == 1L) {
    mbs <- mapply(bsplines, x = at, iknots = iknots, bknots = bknots, order = orders, SIMPLIFY = FALSE)
    tensor <- build_tensor(mbs[-margin])
    thetas <- apply(array(x$cn$theta, dim = dfs), margin, function(x) x)
    marginal_cp <- cp(x$bspline_list[[margin]], t(tensor %*% thetas))

    eval(do.call(call, c(list(name = "plot", x = marginal_cp), cp_args)), environment())

  } else if (length(margin) == 2L) {

    if (show_net) {
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
    }

    if (show_surface) { 
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
    }

    if (show_net) { 
        do.call(rgl::persp3d,
                c(
                  list(x = unique(net[[margin[1]]]),
                       y = unique(net[[margin[2]]]),
                       z = matrix(net[['z']],
                                  nrow = dplyr::n_distinct(net[[margin[1]]]),
                                  ncol = dplyr::n_distinct(net[[margin[2]]]))),
                  net_args)
                )
      if (show_surface) {
        do.call(rgl::persp3d,
                c(
                  list(x = unique(surface[[margin[1]]]),
                       y = unique(surface[[margin[2]]]),
                       z = matrix(surface[['z']],
                                  nrow = dplyr::n_distinct(surface[[margin[1]]]),
                                  ncol = dplyr::n_distinct(surface[[margin[2]]])),
                       add = TRUE),
                  surface_args)
                )
      }

    } else if (!show_net & show_surface) {
        do.call(rgl::persp3d,
                c(
                  list(x = unique(surface[[margin[1]]]),
                       y = unique(surface[[margin[2]]]),
                       z = matrix(surface[['z']],
                                  nrow = dplyr::n_distinct(surface[[margin[1]]]),
                                  ncol = dplyr::n_distinct(surface[[margin[2]]]))),
                  surface_args)
                )
    } else {
      warning("Nothing to plot.")
      return(invisible())
    } 

  } else {
    stop("margin needs to be a length one or two integer vector")
  }
  invisible(list(surface = surface, net = net))
}
