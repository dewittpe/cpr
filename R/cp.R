#' Control Polygons
#'
#' Generate the control polygon for a uni-variable B-spline
#'
#' \code{cp} generates the control polygon for the given B-spline function.  
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param x a \code{cpr_bs} object 
#' @param ... arguments passed to the regression method
#'
#' 
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
#' dat <- dplyr::data_frame(x = xvec, y = sin((x - 2)/pi) + 1.4 * cos(x/pi))
#' cp3 <- cp(y ~ cpr::bsplines(x) + 0, data = dat)
#' 
#' # plot the control polygon, spline and target data.
#' plot(cp3, show_spline = TRUE) + 
#'   ggplot2::geom_line(mapping = ggplot2::aes_string(x = "x", y = "y"), 
#'                      data = dat, linetype = 2, color = "red")
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
  out <- list(cp = dplyr::data_frame(xi_star = as.numeric(attr(x, "xi_star")), 
                                     theta   = as.vector(theta)),
              xi = c(attr(x, "xi")),
              iknots = c(attr(x, "iknots")),
              bknots = c(attr(x, "bknots")),
              order  = attr(x, "order"),
              call   = match.call(),
              keep_fit = NA,
              fit    = NA,
              loglik = NA,
              rmse   = NA)

  class(out) <- c("cpr_cp", class(out))

  out
}

#' @export
#' @rdname cp
#' @param formula a formula that is appropriate for regression method being
#'        used.
#' @param data a required \code{data.frame}
#' @param method the regression method such as \code{\link[stats]{lm}},
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
#' @param keep_fit (logical, default value is \code{FALSE}).  If \code{TRUE} the
#' regression model fit is retained and returned in as the \code{fit} element.
#' If \code{FALSE} the \code{fit} element with be \code{NA}.
#' @param check_rank (logical, defaults to \code{TRUE}) if \code{TRUE} check
#' that the design matrix is full rank.
cp.formula <- function(formula, data, method = stats::lm, ..., keep_fit = FALSE, check_rank = TRUE) { 
  # check for some formula specification issues 
  if (sum(grepl("bsplines", attr(stats::terms(formula), "term.labels"))) != 1) {
    stop("cpr::bsplines() must appear once, with no effect modifiers, on the right hand side of the formula.")
  }

  # this function will add f_for_use and data_for_use into this environment
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(formula, data)

  regression <- match.fun(method)
  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) %in% c("method", "keep_fit", "check_rank")))]
  cl$formula <- as.name("f_for_use")
  cl$data <- as.name("data_for_use")

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
    list(dfs        = length(object$cp$theta),
         n_iknots   = length(object$iknots),
         iknots     = list(object$iknots),
         loglik     = object$loglik,
         rmse       = object$rmse)
  
  
  if (wiggle) {
    wggl <- try(do.call(wiggle.cpr_cp, c(list(object = object), integrate.args)), silent = TRUE)
    

    if (class(wggl) == "integrate") { 
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
#' to be plotted, set this value to TRUE to have the graphic in color (linetypes
#' will be used regardless of the color setting).
#' @param n the number of data points to use for plotting the spline
#'
plot.cpr_cp <- function(x, ..., show_cp = TRUE, show_spline = FALSE, show_xi = TRUE, color = FALSE, n = 100) { 
  nms   <- sapply(match.call()[-1], deparse)
  nms   <- nms[!(names(nms) %in% c("show_cp", "show_spline", "show_xi", "color", "n"))]
  cps   <- lapply(list(x, ...), function(x) x$cp)

  rfctr <- lazyeval::interp( ~ factor(row, levels = seq(1, length(cps)), labels = nms))
  .data <- dplyr::mutate_(dplyr::bind_rows(cps, .id = "row"),
                          .dots = stats::setNames(list(rfctr), "row")) 
  .data <- dplyr::rename_(.data, .dots = 
                          stats::setNames(list( ~ xi_star, ~ theta),
                                          c("x", "y")))

  knot_data <- lapply(list(x, ...), function(x) {data.frame(x = x$xi)})
  knot_data <- dplyr::mutate_(dplyr::bind_rows(knot_data, .id = "row"),
                          .dots = stats::setNames(list(rfctr), "row")) 
  spline_data <- 
    lapply(list(x, ...), function(xx) { 
           b <- xx$bknots
           bmat <- cpr::bsplines(seq(b[1], b[2], length = n), 
                                 iknots = xx$iknots, 
                                 bknots = b, 
                                 order  = xx$order)
           data.frame(x = seq(b[1], b[2], length = n), 
                      y = as.numeric(bmat %*% xx$cp$theta))
                          }) 
  spline_data <- 
    dplyr::mutate_(dplyr::bind_rows(spline_data, .id = "row"),
                   .dots = stats::setNames(list(rfctr), "row")) 

  .data <- dplyr::bind_rows(.data, knot_data, spline_data, .id = 'object')
  .data$object <- factor(.data$object, levels = 1:3, labels = c("cp", "knots", "spline"))

  base_plot <- 
    ggplot2::ggplot(.data) +
    ggplot2::theme_bw() + 
    ggplot2::aes_string(x = "x", y = "y") +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (show_xi) {
    base_plot <-
      base_plot +
      ggplot2::geom_rug(data = function(x) dplyr::filter_(x, .dots = ~ object == "knots")) 
  }

  if (show_cp) {
    base_plot <-
      base_plot +
      ggplot2::geom_point(data = function(x) dplyr::filter_(x, .dots = ~ object == "cp")) +
      ggplot2::geom_line(data = function(x) dplyr::filter_(x, .dots = ~ object == "cp"))
  }

  if (show_spline) { 
    base_plot <- 
      base_plot + 
      ggplot2::geom_line(data = function(x) dplyr::filter_(x, .dots = ~ object == "spline"))
  }

  if (length(cps) > 1) { 
    base_plot <- 
      base_plot + 
      ggplot2::aes_string(linetype = "row") + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  if (color) { 
    base_plot <-
      base_plot +
      ggplot2::aes_string(color = "row") +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }
      
  base_plot
}

