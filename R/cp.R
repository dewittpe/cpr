#' Control Polygons
#'
#' Generate the control polygon for a univariable B-spline
#'
#' \code{cp} generates the control polygon for the given B-spline function.  
#'
#' \code{cpr} runs the control polygon reduction algorithm
#'
#' @param formula a formula that is appropriate for regression method being
#'        used.
#' @param data see documentation in \code{\link{lm}}
#' @param method the regression method such as \code{\link{lm}},
#'        \code{\link{glm}}, \code{\link{lmer}}, \code{\link{geeglm}}, ...
#' @param ... arguments passed to the regression method
#'


#' @export
cp <- function(formula, data = parent.env(), method = lm, ...) { 
  # test that cpr::bs is in the formula
  if (!isTRUE(is.cpr_bspline(formula))) {
    stop("cpr::bspline() must be part of the right hand side of the formula")
  }
  formula
}

is.cpr_bspline <- function(form) { 
  rr <- function(x) { 
    if (is.call(x) && deparse(x[[1]]) %in% paste0(c("", "cpr::"), "bsplines")) {
      TRUE
    } else if (is.recursive(x)) { 
      lapply(as.list(x), rr)
    } 
  }

  z <- lapply(as.list(form), rr)
  unlist(z)
}
