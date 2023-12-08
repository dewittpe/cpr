#' Control Polygons
#'
#' Generate the control polygon for a uni-variable B-spline
#'
#' \code{cp} generates the control polygon for the given B-spline function.
#'
#' @param x a \code{cpr_bs} object
#' @param ... pass through
#'
#' @examples
#'
#' # Support
#' xvec <- seq(0, 6, length = 500)
#'
#' # Define the basis matrix
#' bmat1 <- cpr::bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5))
#' bmat2 <- cpr::bsplines(x = xvec)
#'
#' # Define the control vertices ordinates
#' theta1 <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
#' theta2 <- c(1, 3.4, -2, 1.7)
#'
#' # build the two control polygons
#' cp1 <- cp(bmat1, theta1)
#' cp2 <- cp(bmat2, theta2)
#'
#' # black and white plot
#' plot(cp1)
#' plot(cp1, show_spline = TRUE)
#'
#' # multiple control polygons
#' plot(cp1, cp2, show_spline = TRUE)
#' plot(cp1, cp2, color = TRUE)
#' plot(cp1, cp2, show_spline = TRUE, color = TRUE)
#'
#' # via formula
#' dat <- data.frame(x = xvec, y = sin((xvec - 2)/pi) + 1.4 * cos(xvec/pi))))
#' cp3 <- cp(y ~ cpr::bsplines(x), data = dat)
#'
#' # plot the spline and target data.
#' plot(cp3, show_cp = FALSE, show_spline = TRUE) +
#'   ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, color = "Target"),
#'                      data = dat, linetype = 2)
#'
#' @export
#' @rdname cp
cp <- function(x, ...) {
  UseMethod("cp")
}

#' @export
#' @rdname cp
#' @param theta a vector of (regression) coefficients, the ordinates of the
#'        control polygon.
cp.cpr_bs <- function(x, theta, ...) {
  out <- list(cp = data.frame(xi_star = as.numeric(attr(x, "xi_star")),
                              theta   = as.vector(theta)),
              xi = c(attr(x, "xi")),
              iknots = c(attr(x, "iknots")),
              bknots = c(attr(x, "bknots")),
              order  = attr(x, "order"),
              call   = match.call(),
              keep_fit = NA,
              fit    = NA,
              theta_vcov = NA,
              coefficients = NA,
              vcov = NA,
              loglik = NA,
              rmse   = NA)

  class(out) <- c("cpr_cp", class(out))

  out
}

#' @export
#' @rdname cp
#' @param formula a formula that is appropriate for regression method being
#' used.
#' @param data a required \code{data.frame}
#' @param method the regression method such as \code{\link[stats]{lm}},
#' \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, etc.
#' @param method.args a list of additional arguments to pass to the regression
#' method.
#' @param keep_fit (logical, default value is \code{FALSE}).  If \code{TRUE} the
#' regression model fit is retained and returned in as the \code{fit} element.
#' If \code{FALSE} the \code{fit} element with be \code{NA}.
#' @param check_rank (logical, defaults to \code{TRUE}) if \code{TRUE} check
#' that the design matrix is full rank.
cp.formula <- function(formula, data, method = stats::lm, method.args = list(), keep_fit = FALSE, check_rank = TRUE, ...) {

  # check for some formula specification issues
  rhs_check <- grepl("bsplines", attr(stats::terms(formula), "term.labels"))
  if ( !rhs_check[1] | any(rhs_check[-1]) ) {
    stop("bsplines() must appear once, with no effect modifiers, as the first term on the right hand side of the formula.")
  }

  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  regression <- match.fun(method)
  #cl <- as.list(match.call())
  #cl <- cl[-c(1, which(names(cl) %in% c("method", "keep_fit", "check_rank")))]
  #cl$formula <- as.name("f_for_use")
  #cl$data <- as.name("data_for_use")
  cl <- list(formula = as.name("f_for_use"),
             data = as.name("data_for_use"))
  cl <- c(cl, method.args)

  fit <- do.call(regression, cl)

  if (check_rank) {
    m <- stats::model.matrix(lme4::nobars(f_for_use), data_for_use)
    if (matrix_rank(m) != ncol(m) | any(is.na(BETA(fit)))) {
      warning("Design Matrix is rank deficient. keep_fit being set to TRUE.",
              call. = FALSE,
              immediate. = TRUE)
    keep_fit <- TRUE
    }
  }

  cl <- as.list(match.call())
  cl[[1]] <- as.name("cp")
  cl <- as.call(cl)

  Bmat <- stats::model.frame(fit)
  Bmat <- Bmat[[which(grepl("bsplines", names(Bmat)))]]

  out <- cp.cpr_bs(Bmat, as.vector(theta(fit)))

  out$call         <- cl
  out$keep_fit     <- keep_fit
  out$fit          <- if (keep_fit) { fit } else {NA}
  out$theta_vcov   <- SIGMA(fit)[1:length(out$cp$theta), 1:length(out$cp$theta)]
  out$coefficients <- BETA(fit)
  out$vcov         <- SIGMA(fit)
  out$loglik       <- loglikelihood(fit)
  out$rmse         <- sqrt(mean(stats::residuals(fit)^2))

  out
}

#' @method print cpr_cp
#' @export
#' @rdname cp
print.cpr_cp <- function(x, ...) {
  print(x$cp, ...)
  invisible(x)
}

#' @export
#' @param object a \code{cpr_cp} object
#' @param wiggle logical, if \code{TRUE} then the integral of the squared second
#' derivative of the spline function will be calculated via
#' \code{stats::integrate}.
#' @param integrate.args a list of arguments passed to \code{cpr::wiggle} and
#' ultimately \code{stats::integrate}.
#' @rdname cp
summary.cpr_cp <- function(object, wiggle = FALSE, integrate.args = list(), ...){
  out <-
    data.frame(dfs        = length(object$cp$theta),
         n_iknots   = length(object$iknots),
         iknots     = I(list(object$iknots)),
         loglik     = object$loglik,
         rmse       = object$rmse)

  if (wiggle) {
    wggl <- try(do.call(wiggle.cpr_cp, c(list(object = object), integrate.args)), silent = TRUE)


    if (inherits(x = wggl, what = "integrate")) {
      out$wiggle <- as.numeric(wggl$value)
      attr(out$wiggle, "abs.error") <- wggl$abs.error
      attr(out$wiggle, "subdivisions") <- wggl$subdivisions
      attr(out$wiggle, "message") <- wggl$message
    } else {
      out$wiggle <- NA
      attr(out$wiggle, "error") <- wggl
    }
  }
  out
}


#' @method plot cpr_cp
#' @export
#' @rdname cp
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
#' @param comparative when \code{TRUE} use \code{color} to distinquish one spline from
#' another, when \code{FALSE} \code{color} to highight the control polygon and
#' spline with different colors, and plot the knots the way
#' \code{\link{plot.cpr_bs}} does.  When missing, the default if \code{TRUE} if
#' more than one \code{cpr_cp} object is passed in, and \code{FALSE} is only one
#' \code{cpr_cp} object is passed.
#' @param digits number of digits to the right of the decimal place to report
#' for the value of each knot. Only used when plotting on control polygon with
#' \code{comparative = FALSE}.
#'
#' @return a ggplot
#'
#' @examples
#'
#' x <- seq(0, 6, length = 500)
#' bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5))
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

  if (comparative) {
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

    if (color) {
      base_plot <-
        base_plot +
        eval(substitute(ggplot2::aes(linetype = GRP, color = GRP), list(GRP = as.name("object")))) +
        ggplot2::scale_color_discrete(labels = scales::parse_format())
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

