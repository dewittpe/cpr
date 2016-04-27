#' Control Polygon Reduction
#'
#' Run the Control Polygon Reduction Algorithm.
#'
#' \code{cpr} runs the control polygon reduction algorithm.  
#'
#' \code{keep} will keep the regression fit as part of the \code{cpr\_cp} object
#' for models with upto and including keep fits.  For example, if \code{keep =
#' 10} then the resulting \code{cpr\_cpr} object will have the regression fit
#' stored in the first \code{keep + 1} (zero internal knots, one internal knot,
#' \ldots, \code{keep} internal knots) \code{cpr\_cp} objects in the list.  The
#' limit on the number of stored regression fits is to keep memory usage down.
#'
#' @param formula a formula that is appropriate for regression method being
#'        used.
#' @param data see documentation in \code{\link[stats]{lm}}
#' @param method the regression method such as \code{\link[stats]{lm}},
#'        \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}, \code{\link[geepack]{geeglm}}, ...
#' @param p defaults to 2L, the L^p norm used in determining the influence
#'        weight of each internal knot.
#' @param keep an integer value (defaults to -1L), the number of regression fits
#' to keep.  See Details.
#' @param ... arguments passed to the regression method
#' 
#' @export
cpr <- function(formula, data = parent.env(), method = lm, p = 2L, keep = -1L, ...) { 

  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) %in% c("p", "keep")))]

  control_polygon <- do.call(cp, cl)
  iknots <- attr(attr(control_polygon, "bmat"), "iknots")

  # return(list(control_polygon, iknots))

  results <- vector("list", length = length(iknots) + 1L) 
  
  for(i in seq_along(results)) { 
    xi     <- attr(attr(control_polygon, "bmat"), "xi") 
    iknots <- attr(attr(control_polygon, "bmat"), "iknots") 

    if (length(iknots) > 0) { 
      w <- weigh_iknots(xi, control_polygon$theta, attr(attr(control_polygon, "bmat"), "order"), p) 
    } else {
      w <- NA
    }

    if (length(iknots) > keep) { 
      attr(control_polygon, "fit") <- NULL 
    }

    results[[i]] <- control_polygon

    if (length(iknots) > 0) { 
      cl$formula <- (newknots(cl$formula, iknots[-which.min(w)]))
      class(cl$formula) <- NULL
      control_polygon <- do.call(cp, cl)
    }
  }

  results <- results[rev(seq_along(results))]
  class(results) <- c("cpr_cpr", class(results))
  return(results)
}

#' @method print cpr_cpr
#' @export
print.cpr_cpr <- function(x, ...) { 
  cat("A list of control polygons\n")
  str(x, max.level = 0)
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

# is.cpr_bspline <- function(form) { 
#   rr <- function(x) { 
#     if (is.call(x) && grepl("bsplines$", deparse(x[[1]]))) { 
#       TRUE
#     } else if (is.recursive(x)) { 
#       lapply(as.list(x), rr)
#     } else {
#       NULL
#     }
#   }
# 
#   z <- lapply(as.list(form), rr)
#   unlist(z)
# }

