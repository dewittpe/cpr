#' Control Polygons
#'
#' Generate the control polygon for a uni-variable B-spline
#'
#' \code{cp} generates the control polygon for the given B-spline function.  
#'
#' \code{cpr} runs the control polygon reduction algorithm
#'
#' @param x a \code{cpr_bs} object 
#' @param ... arguments passed to the regression method
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
#' @param integrate.args a list of arguments passed to \code{cpr::wiggle} and
#' ultimately \code{stats::integrate}.
cp.cpr_bs <- function(x, theta, integrate.args = list(), ...) {
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
              rmse   = NA,
              wiggle = NA) 

  class(out) <- c("cpr_cp", class(out))

  out$wiggle <- do.call(wiggle.cpr_cp, c(list(object = out), integrate.args))

  out
}

#' @export
#' @rdname cp
#' @param formula a formula that is appropriate for regression method being
#'        used.
#' @param data see documentation in \code{\link[stats]{lm}}
#' @param method the regression method such as \code{\link[stats]{lm}},
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
#' @param keep_fit (logical, default value is \code{FALSE}).  If \code{TRUE} the
#' regression model fit is retained and returned in as the \code{fit} element.
#' If \code{FALSE} the \code{fit} element with be \code{NA}.
cp.formula <- function(formula, data = parent.frame(), method = stats::lm, ..., keep_fit = FALSE, integrate.args = list()) { 
  # check for some formula specification issues 
  if (sum(grepl("bsplines", attr(stats::terms(formula), "term.labels"))) != 1) {
    stop("cpr::bsplines() must appear once, with no effect modifiers, on the right hand side of the formula.")
  }

  # return(factors_characters_in_f(formula, data))
  if (factors_characters_in_f(formula, data)) { 
    stop("At least one factor/character variable in the formula.  Use cpr::generate_cp_formula_data to create a data.frame and formula.")
  }
  
  regression <- match.fun(method)
  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) %in% c("method", "keep_fit", "integrate.args")))]

  if (attr(stats::terms(formula), "intercept") == 1) { 
    cl$formula <- stats::update.formula(formula, . ~ . - 1)
  }

  fit <- do.call(regression, cl)

  cl <- as.list(match.call())
  cl[[1]] <- as.name("cp")
  cl <- as.call(cl)

  Bmat <- stats::model.frame(fit)
  Bmat <- Bmat[[which(grepl("bsplines", names(Bmat)))]]

  out <- list(cp = dplyr::data_frame(xi_star = as.numeric(attr(Bmat, "xi_star")), 
                                     theta   = as.vector(theta(fit))),
              xi       = c(attr(Bmat, "xi")),
              iknots   = c(attr(Bmat, "iknots")),
              bknots   = c(attr(Bmat, "bknots")),
              order    = attr(Bmat, "order"),
              call     = cl,
              keep_fit = keep_fit,
              fit      = if (keep_fit) { fit } else {NA},
              loglik   = loglikelihood(fit),
              rmse     = sqrt(mean(stats::residuals(fit)^2)),
              wiggle   = numeric(0))

  class(out) <- c("cpr_cp", class(out))

  out$wiggle <- do.call(wiggle.cpr_cp, c(list(object = out), integrate.args))

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
#' @rdname cp
summary.cpr_cp <- function(object, ...){
  list(dfs        = length(object$cp$theta),
       n_iknots   = length(object$iknots),
       iknots     = object$iknots,
       loglik     = object$loglik,
       rmse       = object$rmse,
       wiggle     = object$wiggle$value,
       wiggle_msg = object$wiggle$message)
}


#' @method plot cpr_cp
#' @export
#' @rdname cp
#' @param show_spline boolean (default FALSE) to plot the spline function within
#' its control polygon
#' @param color boolean (default FALSE) if more than one \code{cpr_cp} object is
#' to be plotted, set this value to TRUE to have the graphic in color (linetypes
#' will be used regardless of the color setting).
#' @param n the number of data points to use for plotting the spline
#'
plot.cpr_cp <- function(x, ..., show_spline = FALSE, color = FALSE, n = 100) { 
  nms   <- sapply(match.call()[-1], deparse)
  nms   <- nms[!(names(nms) %in% c("show_spline", "color", "n"))]
  cps   <- lapply(list(x, ...), function(x) x$cp)
  rfctr <- lazyeval::interp( ~ factor(row, levels = seq(1, length(cps)), labels = nms))
  .data <- dplyr::mutate_(dplyr::bind_rows(cps, .id = "row"),
                          .dots = stats::setNames(list(rfctr), "row")) 
                  
  base_plot <- 
    ggplot2::ggplot(.data) +
    ggplot2::theme_bw() + 
    ggplot2::geom_point() + 
    ggplot2::geom_line() + 
    ggplot2::theme(axis.title = ggplot2::element_blank())

  if (length(cps) > 1) { 
    base_plot <- 
      base_plot + 
      ggplot2::aes_string(x = "xi_star", y = "theta", linetype = "factor(row)") + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
  } else { 
    base_plot <- 
      base_plot + ggplot2::aes_string(x = "xi_star", y = "theta")
  }

  if (color) { 
    base_plot <-
      base_plot + 
      ggplot2::aes_string(color = "factor(row)") 
  }
      

  if (show_spline) { 
    .data2 <- 
      lapply(list(x, ...), function(xx) { 
           b <- xx$bknots
           bmat <- cpr::bsplines(seq(b[1], b[2], length = n), 
                                 iknots = xx$iknots, 
                                 bknots = b, 
                                 order  = xx$order)
           data.frame(x = seq(b[1], b[2], length = n), 
                      y = as.numeric(bmat %*% xx$cp$theta))
                          }) 
    .data2 <- 
        dplyr::mutate_(dplyr::bind_rows(.data2, .id = "row"),
                      .dots = stats::setNames(list(rfctr), "row")) 

      base_plot <- 
        base_plot + 
        ggplot2::geom_line(data = .data2, 
                           mapping = ggplot2::aes_string(x = "x", y = "y"))
  }
  base_plot
}

