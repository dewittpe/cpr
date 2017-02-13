#' Control Net Reduction Plots
#'
#' A collection of function for the inspection and evaluation of the control
#' polygon reduction.
#' 
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @method plot cpr_cnr
#' @rdname cnr_diagnostics
#' @export 
#' @param x a \code{cpr_cnr} object
#' @param type type of diagnostic plot.
#' \code{"loglik"} for the loglikihood by degrees of freedom, 
#' \code{"rmse"} for root mean squared residuals by model index
#' @param from the first index of \code{x} to plot
#' @param to the last index of \code{x} to plot
#' @param ... ignored
plot.cpr_cnr <- function(x, type = "rmse", from = 1, to, ...) { 

  if (from < 1) { 
    from <- 1
  } 

  if (any(type %in% c("rmse", "loglik"))) { 
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
    stop("type needs to be either 'loglik' or 'rmse'.")
  }
}

