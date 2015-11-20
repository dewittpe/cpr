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

    results[[i]] <- list(iknots = iknots, 
                         wts = w,
                         removed = iknots[which.min(w)],
                         fit = control_polygon$fit)

    
    control_polygon <- cp(newknots(formula, iknots[-which.min(w)]), data = data, method = method, ...) 
  }
  return(results)
}



# cbind(iknots, w)



  # if (length(rm_xi) < 1) { 
  #   control_polygon
  # } else { 
  #   new_iknots <- attr(control_polygon$Bmat, "iknots")
  #   new_iknots <- new_iknots[-which(new_iknots == rm_xi)] 
  #   cpr(newknots(formula, new_iknots), data = data, method = method, psi_f = psi_f, psi_n = psi_n, K = K, ...) 
  # }
# }

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
