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
  fit <- regression(formula, data = data, ...)

  # extract bspline
  eval(extract_cpr_bspline(formula), data)
}

is.cpr_bspline <- function(form) { 
  rr <- function(x) { 
    if (is.call(x) && grepl("bsplines$", deparse(x[[1]]))) { 
      TRUE
    } else if (is.recursive(x)) { 
      lapply(as.list(x), rr)
    } else {
      NULL
    }
  }

  z <- lapply(as.list(form), rr)
  unlist(z)
}

extract_cpr_bspline <- function(form) { 
  B <- NULL
  rr <- function(x) { 
    if (is.call(x) && grepl("bsplines", deparse(x[[1]]))) { 
      # assign(B, value = as.call(x), inherits = TRUE)
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
