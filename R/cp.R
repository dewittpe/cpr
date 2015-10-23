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
#'        \code{\link[glm]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
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
  Bmat <- eval(extract_cpr_bspline(formula), data)

  cp <- data.frame(xi_star   = attr(Bmat, "xi_star"), 
                   theta     = theta(fit), 
                   convexity = NA, 
                   fair      = NA,
                   psi       = NA,
                   rm_xi     = c(NA, min(attr(Bmat, "iknots")), attr(Bmat, "iknots"), max(attr(Bmat, "iknots")), NA),
                   row.names = seq(1, ncol(Bmat), by = 1))

  for(i in 2:(nrow(cp) - 1)) { 
    cp$convexity[i] <-
      sign((
            diff(cp$theta[i + c(-1, 1)]) / 
            diff(cp$xi_star[i + c(-1, 1)]) * 
            diff(cp$xi_star[i + c(-1, 0)]) + 
            cp$theta[i - 1]
           ) - 
           cp$theta[i] 
    ) 
  }

  cp$fair <- (cp$convexity == dplyr::lag(cp$convexity, 1L) | cp$convexity == dplyr::lead(cp$convexity, 1L))
  cp$fair[c(1, 2, nrow(cp) - c(0, 1))] <- NA

  cp$psi <- c(NA, 
      sapply(2:(nrow(cp) - 1L), 
             function(i) { 
               x1 <- diff(cp[i + c(0, -1), "xi_star"])
               y1 <- diff(cp[i + c(0, -1), "theta"])
               x2 <- diff(cp[i + c(0, 1),  "xi_star"])
               y2 <- diff(cp[i + c(0, 1),  "theta"]) 
               acos(((x1 * x2) + (y1 * y2)) / (sqrt(x1**2 + y1**2) * sqrt(x2**2 + y2**2))) * 180 / pi
             }), NA) 


  out <- list(cp = cp, Bmat = Bmat)
  class(out) <- "cpr_cp"
  out 
}

#' @export
#' @rdname cp
#' @param psi_f the limit for effective colinear fair points
#' @param psi_n the limit for effective colinear non-fair points
#' @param K reduce the control polygon, regardels of psi_f and psi_n untill K or
#' fewer internal knots are left.
cpr <- function(formula, data = parent.env(), method = lm, psi_f = 170, psi_n = 165, K = 0, ...) { 
  control_polygon <- cp(formula, data, method, ...) 

  if (attr(control_polygon$Bmat, "order") != 4) {
    stop("Reduction method only implimented for cubic (degree = 3, order = 4) splines.")
  }

  kill <- dplyr::ungroup(dplyr::filter(dplyr::group_by(control_polygon$cp, fair), psi == max(psi)))
  rm_xi <- dplyr::filter(kill, !fair, psi > psi_n)$rm_xi

  if (length(rm_xi) < 1) {
    if (length(attr(control_polygon$Bmat, "iknots")) > K) { 
      rm_xi <- dplyr::filter(kill, psi == max(psi))$rm_xi
    } else { 
      rm_xi <- dplyr::filter(kill, psi > psi_f)$rm_xi
    }
  } 

  if (length(rm_xi) < 1) { 
    control_polygon
  } else { 
    new_iknots <- attr(control_polygon$Bmat, "iknots")
    new_iknots <- new_iknots[-which(new_iknots == rm_xi)] 
    cpr(newknots(formula, new_iknots), data = data, method = method, psi_f = psi_f, psi_n = psi_n, K = K, ...) 
  }
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
  rr <- function(x, nk, calls) {
      if(is.call(x) && grepl("bsplines", deparse(x[[1]]))) {
          x$iknots <- nk
          x
      } else if (is.recursive(x)) {
          as.call(lapply(as.list(x), rr, nk, calls))
      } else {
          x
      }
  }

  z <- lapply(as.list(form), rr, nk, calls)   
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
