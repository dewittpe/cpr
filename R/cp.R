#' Control Polygons
#'
#' Generate the control polygon for a uni-variable B-spline
#'
#' \code{cp} generates the control polygon for the given B-spline function.  
#'
#' \code{cpr} runs the control polygon reduction algorithm
#'
#' @param formula a formula that is appropriate for regression method being
#'        used.
#' @param data see documentation in \code{\link[stats]{lm}}
#' @param method the regression method such as \code{\link[stats]{lm}},
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
#' @param ... arguments passed to the regression method
#'
#' @export
cp <- function(formula, data = parent.env(), method = lm, ...) { 
 
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

  attr(out, "iknots") <- c(attr(Bmat, "iknots"))
  attr(out, "bknots") <- c(attr(Bmat, "bknots"))
  attr(out, "xi")     <- c(attr(Bmat, "xi"))
  attr(out, "order")  <- attr(Bmat, "order") 
  attr(out, "call") <- match.call() 

  class(out) <- c("cpr_cp", class(out))
  out 
}

#' @method print cpr_cp
#' @export
print.cpr_cp <- function(x, ...) { 
  #dplyr::print(x, ...)
  print(x, ...)
}

#' @method plot cpr_cp
#' @export
plot.cpr_cp <- function(x, ..., show_spline = FALSE, n = 500) { 
  nms   <- sapply(match.call()[-1], deparse)
  nms   <- nms[!(names(nms) %in% c("show_spline", "n"))]
  cps   <- list(x, ...)
  rfctr <- lazyeval::interp( ~ factor(row, levels = seq(1, length(cps)), labels = nms))
  .data <- dplyr::mutate_(dplyr::bind_rows(cps, .id = "row"),
                          .dots = setNames(list(rfctr), "row")) 
                  
  base_plot <- 
    ggplot2::ggplot(.data) +
    ggplot2::theme_bw() + 
    ggplot2::geom_point() + 
    ggplot2::geom_line() 

  if (length(cps) > 1) { 
    base_plot <- 
      base_plot + 
      ggplot2::aes_string(x = "xi_star", y = "theta", linetype = "factor(row)") + 
      ggplot2::scale_linetype(name = ggplot2::element_blank())
  } else { 
    base_plot <- 
      base_plot + ggplot2::aes_string(x = "xi_star", y = "theta")
  }

  if (show_spline) { 
    .data2 <- 
      lapply(cps, function(x) { 
           b <- attr(x, "bknots")
           bmat <- bsplines(seq(b[1], b[2], length = n), 
                            iknots = attr(x, "iknots"), 
                            bknots = b, 
                            order  = attr(x, "order"))
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


