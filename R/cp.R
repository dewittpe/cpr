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

  out <- dplyr::data_frame(xi_star = as.numeric(attr(Bmat, "xi_star")), 
                           theta   = theta(fit)) 
  # names(attributes(bmat))
  # out <- list(cp = cp, Bmat = Bmat, fit = fit)
  attr(out, "iknots") <- c(attr(Bmat, "iknots"))
  attr(out, "bknots") <- c(attr(Bmat, "bknots"))
  attr(out, "xi")     <- c(attr(Bmat, "xi"))
  attr(out, "order")  <- attr(Bmat, "order") 
  attr(out, "call") <- match.call() 

  attr(out, "method") <- regression
  attr(out, "formula") <- formula
  attr(out, "data")   <- data
  attr(out, "dots")   <- list(...)

  class(out) <- c("cpr_cp", class(out))
  out 
}

#' @export
#' @rdname cp
#' @param p defaults to 2L, the L^p norm used in determining the 'weight of
#' importance' of each internal knot.
cpr <- function(formula, data = parent.env(), method = lm, p = 2L, ...) { 
  control_polygon <- cp(formula, data, method, ...) 
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
      control_polygon <- cp(newknots(formula, iknots[-which.min(w)]), data = data, method = method, ...) 
    }
  }

  results <- results[rev(seq_along(results))]
  class(results) <- c("cpr_cpr", class(results))
  return(results)
}

#' @method print cpr_cp
#' @export
print.cpr_cp <- function(x, ...) { 
  dplyr:::print.tbl_df(x, ...)
}

#' @method print cpr_cpr
#' @export
print.cpr_cpr <- function(x, ...) { 
  cat("A list of control polygons\n")
  str(reduction, max.level = 0)
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
                             }) %>%
      {
        dplyr::mutate_(dplyr::bind_rows(., .id = "row"),
                      .dots = setNames(list(rfctr), "row")) 
      }

      base_plot <- 
        base_plot + 
        ggplot2::geom_line(data = .data2, 
                           mapping = ggplot2::aes_string(x = "x", y = "y"))
  }
  base_plot
}

#' @export
model <- function(x) { 
  UseMethod("model")
}

#' @export
model.cpr_cp <- function(x) { 
  do.call(attr(x, "method"), 
          c(list(data = attr(x, "data"), formula = attr(x, "formula")),
            attr(x, "dots"))
          )
}

#' @export
model.cpr_selected <- function(x) { 
  model.cpr_cp(x$best_cp)
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
