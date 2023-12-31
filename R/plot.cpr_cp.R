#' Plotting Control Polygons
#'
#' Plotting control polygon(s) and/or the associated spline(s) via ggplot2
#'
#' @param x a \code{cpr_cp} object
#' @param ... additional \code{cpr_cp} objects
#' @param show_cp logical (default \code{TRUE}), show the control polygon(s)?
#' @param show_spline logical (default \code{FALSE}) to plot the spline
#' function?
#' @param show_xi logical (default \code{TRUE}) use
#' \code{\link[ggplot2]{geom_rug}} to show the location of the knots in the
#' respective control polygons.
#' @param color Boolean (default FALSE) if more than one \code{cpr_cp} object is
#' to be plotted, set this value to TRUE to have the graphic in color (line types
#' will be used regardless of the color setting).
#' @param n the number of data points to use for plotting the spline
#' @param comparative when \code{TRUE} use \code{color} to distinguish one spline from
#' another, when \code{FALSE} \code{color} to highlight the control polygon and
#' spline with different colors, and plot the knots the way
#' \code{\link{plot.cpr_bs}} does.  When missing, the default if \code{TRUE} if
#' more than one \code{cpr_cp} object is passed in, and \code{FALSE} is only one
#' \code{cpr_cp} object is passed.
#' @param show_x boolean, so x-values
#' @param digits number of digits to the right of the decimal place to report
#' for the value of each knot. Only used when plotting on control polygon with
#' \code{comparative = FALSE}.
#'
#' @return a ggplot object
#'
#' @examples
#'
#' x <- runif(n = 500, 0, 6)
#' bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
#' theta1 <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
#' theta2 <- theta1 + c(-0.15, -1.01, 0.37, 0.19, -0.53, -0.84, -0.19, 1.15, 0.17)
#' cp1 <- cp(bmat, theta1)
#' cp2 <- cp(bmat, theta2)
#'
#' # compare two control polygons on one plot
#' plot(cp1, cp2)
#' plot(cp1, cp2, color = TRUE)
#' plot(cp1, cp2, color = TRUE, show_spline = TRUE)
#' plot(cp1, cp2, color = TRUE, show_cp = FALSE, show_spline = TRUE)
#'
#' # Show one control polygon with knots on the axis instead of the rug and
#' # color/linetype for the control polygon and spline, instead of different
#' # control polygons
#' plot(cp1, comparative = FALSE)
#' plot(cp1, comparative = FALSE, show_spline = TRUE)
#' plot(cp1, comparative = FALSE, show_spline = TRUE, show_x = TRUE)
#' plot(cp2, comparative = FALSE, show_spline = TRUE, show_x = TRUE)
#'
#'
#' @method plot cpr_cp
#' @export
plot.cpr_cp <- function(x, ..., comparative, show_cp = TRUE, show_spline = FALSE, show_xi = TRUE, color = FALSE, n = 100, show_x = FALSE, digits = 2) {
  nms <- as.list(match.call(expand.dots = FALSE))
  nms <- unlist(c(nms["x"], nms["..."]))
  nms <- sapply(nms, deparse)

  cps       <- lapply(list(x, ...), getElement, "cp")
  knot_data <- lapply(list(x, ...), function(x) {data.frame(x = x$xi)})
  spline_data <-
    lapply(list(x, ...), function(xx) {
           b <- xx$bknots
           bmat <- bsplines(seq(b[1], b[2] - 1/n, length = n),
                            iknots = xx$iknots,
                            bknots = b,
                            order  = xx$order)
           data.frame(x = seq(b[1], b[2] - 1/n, length = n),
                      y = as.numeric(bmat %*% xx$cp$theta))
                          })

  for(i in seq_along(nms)) {
    cps[[i]]$row <- nms[i]
    knot_data[[i]]$row <- nms[i]
    spline_data[[i]]$row <- nms[i]
  }

  if (missing(comparative)) {
    comparative <- length(cps) > 1L
  }

  if (length(cps) > 1L & !comparative) {
    warning("More than one control polygon to plot, forcing comparative to TRUE")
    comparative <- TRUE
  }

  cps <- do.call(rbind, cps)
  knot_data <- do.call(rbind, knot_data)
  spline_data <- do.call(rbind, spline_data)

  names(cps) <- c("x", "y", "row")
  knot_data$y <- NA_real_

  cps$row       <- factor(cps$row, levels = nms)
  knot_data$row <- factor(knot_data$row, levels = nms)
  spline_data$row <- factor(spline_data$row, levels = nms)

  cps$object <- 1
  knot_data$object <- 2
  spline_data$object <- 3
  plot_data <- rbind(cps, knot_data, spline_data)

  plot_data$object <- factor(plot_data$object, levels = 1:3, labels = c("cp", "knots", "spline"))

  base_plot <-
    ggplot2::ggplot(data = plot_data) +
    ggplot2::theme_bw() +
    eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("x"), Y = as.name("y")))) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (comparative) {
    if (show_cp) {
      base_plot <-
        base_plot +
        eval(substitute(ggplot2::aes(shape = PCH), list(PCH = as.name("row")))) +
        ggplot2::geom_point(data = subset(plot_data, plot_data$object == "cp")) +
        ggplot2::geom_line(data = subset(plot_data, plot_data$object == "cp"))
    }

    if (show_spline) {
      base_plot <-
        base_plot +
        ggplot2::geom_line(data = subset(plot_data, plot_data$object == "spline"))
    }


    if (show_xi) {
      base_plot <-
        base_plot +
        ggplot2::geom_rug(data = subset(plot_data, plot_data$object == "knots"))
    }

    if (length(cps) > 1) {
      base_plot <-
        base_plot +
        eval(substitute(ggplot2::aes(linetype = LTY), list(LTY = as.name("row")))) +
        ggplot2::theme(legend.title = ggplot2::element_blank())
    }

    if (color) {
      base_plot <-
        base_plot +
        eval(substitute(ggplot2::aes(color = CLR), list(CLR = as.name("row")))) +
        ggplot2::theme(legend.title = ggplot2::element_blank())
    }
  } else {
    base_plot <-
      base_plot +
      eval(substitute(ggplot2::aes(linetype = GRP), list(GRP = as.name("object"))))

    if (show_cp) {
      base_plot <-
        base_plot +
        ggplot2::geom_point(data = subset(plot_data, plot_data$object == "cp")) +
        ggplot2::geom_line(data = subset(plot_data, plot_data$object == "cp"))
    }

    if (show_spline) {
      base_plot <-
        base_plot +
        ggplot2::geom_line(data = subset(plot_data, plot_data$object == "spline"))
    }


    if (color) {
      base_plot <-
        base_plot +
        eval(substitute(ggplot2::aes(linetype = GRP, color = GRP), list(GRP = as.name("object"))))
    } else {
      base_plot <-
        base_plot +
        eval(substitute(ggplot2::aes(linetype = GRP), list(GRP = as.name("object"))))
    }

    if (show_xi | show_x) {
      e <- knot_expr(x, digits)
      if (show_xi & !show_x) {
        base_plot <-
          base_plot + ggplot2::scale_x_continuous(breaks = e$breaks,
                                                  labels = parse(text = e$xi_expr),
                                                  minor_breaks = NULL)
      } else if (!show_xi & show_x) {
        base_plot <- base_plot + ggplot2::scale_x_continuous(breaks = e$breaks,
                                                             labels = e$num_expr,
                                                             minor_breaks = NULL)
      } else {
        base_plot <- base_plot + ggplot2::scale_x_continuous(breaks = e$breaks,
                                                             labels = parse(text = e$xi_expr),
                                                             minor_breaks = NULL,
                                                             sec.axis = ggplot2::sec_axis(~ .,
                                                                                          breaks = e$breaks,
                                                                                          labels = e$num_expr))
      }
    }
  }

  base_plot
}
