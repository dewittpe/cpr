#' Control Polygon Reduction
#'
#' Run the Control Polygon Reduction Algorithm.
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
#' @param p defaults to 2L, the L^p norm used in determining the 'weight of
#' importance' of each internal knot.
cpr <- function(formula, data = parent.env(), method = lm, p = 2L, ...) { 

  cl <- as.list(match.call())
  cl <- cl[-c(1, which(names(cl) == "p"))]

  control_polygon <- do.call(cp, cl)#cp(formula, data, method, ...) 
  iknots <- attr(control_polygon, "iknots") 
  results <- vector("list", length = length(iknots) + 1L) 
  
  for(i in seq_along(results)) { 
    xi     <- attr(control_polygon, "xi") 
    iknots <- attr(control_polygon, "iknots") 

    if (length(iknots) > 0) { 
      w <- weigh_iknots(xi, control_polygon$theta, attr(control_polygon, "order"), p) 
    } else {
      w <- NA
    }

    attr(control_polygon, "weights") = w
    attr(control_polygon, "removed") = if (length(iknots) > 0) { c(index = which.min(w), value = iknots[which.min(w)]) } else {NA}

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

