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
  fit <- regression(formula, data = data, ...)

  # extract bspline
  Bmat <- eval(extract_cpr_bspline(formula), data, environment(formula))

  cp <- data.frame(xi_star   = attr(Bmat, "xi_star"), 
                   theta     = theta(fit)) 
  out <- list(cp = cp, Bmat = Bmat, fit = fit)
  class(out) <- c("cpr_cp", class(out))
  out 
}

#' @export
#' @rdname cp
#' @param p defaults to 2L, the L^p norm used in determining the 'weight of
#' importance' of each internal knot.
cpr <- function(formula, data = parent.env(), method = lm, p = 2L, ...) { 
  control_polygon <- cp(formula, data, method, ...) 
  iknots <- attr(control_polygon$Bmat, "iknots") 
  results <- vector("list", length = length(iknots))
  
  for(i in 1:length(results)) { 
    xi     <- attr(control_polygon$Bmat, "xi") 
    iknots <- attr(control_polygon$Bmat, "iknots") 
    w      <- weigh_iknots(xi, control_polygon$cp$theta, attr(control_polygon$Bmat, "order"), p)

    results[[i]] <- c(list(wts = w,
                           removed = iknots[which.min(w)]),
                      control_polygon)

                         # fit = control_polygon$fit)

    
    control_polygon <- cp(newknots(formula, iknots[-which.min(w)]), data = data, method = method, ...) 
  }
  class(results) <- c("cpr_cpr", class(results))
  return(results)
}

#' @export
#' @rdname cp
#' @param obj a cpr_cp object
cp_value <- function(x, obj) { 
  xi_star <- obj$cp[[1]]
  theta   <- obj$cp[[2]]
  
  idx <- min(which(xi_star >= x)) + as.numeric(x == min(xi_star))

  (theta[idx] - theta[idx - 1L]) / (xi_star[idx] - xi_star[idx - 1L]) * (x - xi_star[idx]) + theta[idx] 
}

#' @export
#' @rdname cp
cp_diff <- function(cp1, cp2) { 
  (sapply(cp1$xi_star, cp_values, this_cp = cp2) - cp1$theta) / (diff(range(cp2$theta)))
}

#' @export
#' @rdname cp
#' @param obj a cpr_cpr object
#' @param err max difference/error between the vertices of cp1 to cp2
cpr_select <- function(obj, err = 0.01) { 
  diffs <- mapply(function(x1, x2) { cp_diff(x1$cp, x2$cp) }, 
                  x1 = obj[-length(obj)],
                  x2 = obj[-1])
  min(which(!sapply(diffs, function(x) { all(x < err) })))
}



print.cpr_cp <- function(x, ...) { 
  x$cp
}

plot.cpr_cp <- function(x, y, ...) { 
  list(
       ggplot2::geom_point(data = x$cp, mapping = ggplot2::aes_string(x = "xi_star", y = "theta")),
       ggplot2::geom_line(data = x$cp, mapping = ggplot2::aes_string(x = "xi_star", y = "theta")),
       ggplot2::geom_line(data = data.frame(x = attr(x$Bmat, "x"), 
                                            y = as.numeric(x$Bmat %*% x$cp$theta)), 
                          mapping = ggplot2::aes_string(x = "x", y = "y")) 
       )
}


newknots <- function(form, nk) { 
  rr <- function(x, nk) {
      if(is.call(x) && grepl("bsplines", deparse(x[[1]]))) {
          x$iknots <- nk
          x
      } else if (is.recursive(x)) {
          as.call(lapply(as.list(x), rr, nk))
      } else {
          x
      }
  }

  z <- lapply(as.list(form), rr, nk)   
  z <- eval(as.call(z))
  environment(z) <- environment(form)
  z
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
  out[grepl("bspline", names(out))]
}

theta.glm <- function(fit) { 
  out <- coef(fit)
  out[grepl("bspline", names(out))]
}

theta.lmerMod <- function(fit) { 
  out <- lme4::fixef(fit)
  out[grepl("bspline", names(out))]
}

theta.geeglm <- function(fit) { 
  out <- coef(fit)
  out[grepl("bspline", names(out))]
}
