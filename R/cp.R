#' Control Polygons
#'
#' Generate the control polygon for a uni-variable B-spline
#'
#' \code{cp} generates the control polygon for the given B-spline function.  
#'
#' \code{cpr} runs the control polygon reduction algorithm
#'
#' @param x a \code{cpr_bs} object 
#' @param theta a vector of (regression) coefficients, the ordinates of the
#'        control polygon.
#' @param formula a formula that is appropriate for regression method being
#'        used.
#' @param data see documentation in \code{\link[stats]{lm}}
#' @param method the regression method such as \code{\link[stats]{lm}},
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
#' @param ... arguments passed to the regression method
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
#' # Define the control vertice ordinates
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
#' # plot the control polygon, spline and traget data.
#' plot(cp3, show_spline = TRUE) + 
#'   ggplot2::geom_line(mapping = ggplot2::aes_string(x = "x", y = "y"), 
#'                      data = dat, linetype = 2, color = "red")
#' 
#' @export
cp <- function(x, ...) { 
  UseMethod("cp")
}

#' @export
cp.cpr_bs <- function(x, theta, ...) { 
  out <- dplyr::data_frame(xi_star = as.numeric(attr(x, "xi_star")), 
                           theta   = as.vector(theta))
  attr(out, "bmat") <- x
  class(out) <- c("cpr_cp", class(out))
  out
}

#' @export
cp.formula <- function(formula, data = parent.env(), method = lm, ...) { 
 
  # check for some formula specification issues
  fterms <- terms(formula)
  if (sum(grepl("bsplines", attr(fterms, "term.labels"))) != 1) {
    stop("cpr::bspline() must apear once, with no effect modifiers, on the right hand side of the formula.")
  }

  if (attr(fterms, "intercept")) {
    warning("Adjusting model formula; removing intercept.")
    formula <- stats::update(formula, . ~ . - 1)
  }

  regression <- match.fun(method)
  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) == "method"))]
  fit <- do.call(regression, cl)

  # extract bspline
  Bmat <- eval(extract_cpr_bspline(formula), data, environment(formula))

  out <- dplyr::data_frame(xi_star = as.numeric(attr(Bmat, "xi_star")), 
                           theta   = theta(fit)) 

  # attr(out, "iknots") <- c(attr(Bmat, "iknots"))
  # attr(out, "bknots") <- c(attr(Bmat, "bknots"))
  # attr(out, "xi")     <- c(attr(Bmat, "xi"))
  # attr(out, "order")  <- attr(Bmat, "order") 
  attr(out, "bmat") <- Bmat
  attr(out, "call") <- match.call() 

  class(out) <- c("cpr_cp", class(out))
  out 
}

#' @method print cpr_cp
#' @export
print.cpr_cp <- function(x, ...) { 
  print.data.frame(x, ...)
}

#' @method plot cpr_cp
#' @export
plot.cpr_cp <- function(x, ..., show_spline = FALSE, color = FALSE, n = 500) { 
  nms   <- sapply(match.call()[-1], deparse)
  nms   <- nms[!(names(nms) %in% c("show_spline", "color", "n"))]
  cps   <- list(x, ...)
  rfctr <- lazyeval::interp( ~ factor(row, levels = seq(1, length(cps)), labels = nms))
  .data <- dplyr::mutate_(dplyr::bind_rows(cps, .id = "row"),
                          .dots = setNames(list(rfctr), "row")) 
                  
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
      # ggplot2::scale_linetype(name = ggplot2::element_blank())
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
      lapply(cps, function(x) { 
           b <- attr(attr(x, "bmat"), "bknots")
           bmat <- cpr::bsplines(seq(b[1], b[2], length = n), 
                            iknots = attr(attr(x, "bmat"), "iknots"), 
                            bknots = b, 
                            order  = attr(attr(x, "bmat"), "order"))
           data.frame(x = seq(b[1], b[2], length = n), 
                      y = as.numeric(bmat %*% x$theta))
                             }) 
    .data2 <- 
        dplyr::mutate_(dplyr::bind_rows(.data2, .id = "row"),
                      .dots = setNames(list(rfctr), "row")) 

      base_plot <- 
        base_plot + 
        ggplot2::geom_line(data = .data2, 
                           mapping = ggplot2::aes_string(x = "x", y = "y"))
  }
  base_plot
}


# #' @export
# model <- function(x) { 
#   UseMethod("model")
# }
# 
# #' @export
# model.cpr_cp <- function(x) { 
#   do.call(attr(x, "method"), 
#           c(list(data = attr(x, "data"), formula = attr(x, "formula")),
#             attr(x, "dots"))
#           )
# }
# 
# #' @export
# model.cpr_selected <- function(x) { 
#   model.cpr_cp(x$best_cp)
# }

extract_cpr_bspline <- function(form) { 
  B <- NULL
  rr <- function(x) { 
    if (is.call(x) && grepl("bsplines", deparse(x[[1]]))) { 
      B <<- x
    } else if (is.recursive(x)) { 
      as.call(lapply(as.list(x), rr))
    } else {
      x
    }
  }

  z <- lapply(as.list(form), rr)
  B
}


theta <- function(fit) { 
  UseMethod("theta")
}

theta.lm <- function(fit) { 
  out <- coef(fit)
  unname(out[grepl("bspline", names(out))])
}

theta.glm <- function(fit) { 
  out <- coef(fit)
  unname(out[grepl("bspline", names(out))])
}

theta.lmerMod <- function(fit) { 
  out <- lme4::fixef(fit)
  unname(out[grepl("bspline", names(out))])
}

theta.geeglm <- function(fit) { 
  out <- coef(fit)
  unname(out[grepl("bspline", names(out))])
}
