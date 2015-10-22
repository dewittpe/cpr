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
  Bmat <- eval(extract_cpr_bspline(formula), data)

  cp <- data.frame(xi_star   = attr(Bmat, "xi_star"), 
                   theta     = theta(fit), 
                   convexity = NA, 
                   fair      = NA,
                   psi       = NA,
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

plot.cpr_cp <- function(x, y, ...) { 
  list(
       ggplot2::geom_point(data = x$cp, mapping = ggplot2::aes_string(x = "xi_star", y = "theta")),
       ggplot2::geom_line(data = x$cp, mapping = ggplot2::aes_string(x = "xi_star", y = "theta")),
       ggplot2::geom_line(data = data.frame(x = attr(x$Bmat, "x"), 
                                            y = as.numeric(x$Bmat %*% x$cp$theta)), 
                          mapping = ggplot2::aes_string(x = "x", y = "y")) 
       )
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
  out <- fixef(fit)
  out[grepl("bspline", names(out))]
}

theta.geeglm <- function(fit) { 
  out <- coef(fit)
  out[grepl("bspline", names(out))]
}
