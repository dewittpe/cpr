#' Control Polygon Reduction Plots
#'
#' A collection of function for the inspection and evaluation of the control
#' polygon reduction.
#' 
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'



#' @method plot cpr_cpr
#' @rdname cpr_diagnostics
#' @export 
#' @param x a \code{cpr_cpr} object
#' @param type type of diagnostic plot.  \code{"cps"} for control polygons,
#' \code{"loglik"} for the loglikihood by degrees of freedom, 
#' \code{"rmse"} for root mean squared residuals by degrees of freedom
#' @param from the first index of \code{x} to plot
#' @param to the last index of \code{x} to plot
#' @param ... args passed to \code{cpr::plot.cpr_cp}
#' @seealso \code{\link{plot.cpr_cp}}
plot.cpr_cpr <- function(x, type = "cps", from = 1, to, ...) { 

  if (from < 1) { 
    from <- 1
  } 

  if (type == "cps") { 

    if (missing(to)) { 
      to <- 6
    } else if (to > length(x)) { 
      to <- length(x)
    }

    nm <- deparse(substitute(x))

    eval(parse(text = paste("plot(",
                            paste(paste0(nm, "[[", seq(from = from, to = to, by = 1), "]]"), 
                                  collapse = ", "),
                            ", ...)")))

  } else if (any(type %in% c("rmse", "loglik"))) { 
    if (missing(to)) { 
      to <- length(x)
    } else if (to > length(x)) { 
      to <- length(x)
    }

    ggplot2::ggplot(dplyr::filter_(summary(x), .dots = ~ dplyr::between(index, from, to))) + 
    ggplot2::theme_bw() + 
    ggplot2::aes_string(x = "index", y = type) + 
    ggplot2::geom_point() + 
    ggplot2::geom_line() +
    ggplot2::xlab("Index")

  } else { 
    stop("type needs to be either 'cps', 'loglik', or 'rmse'.")
  }
}

