#' Influence Of Internal Knots
#'
#' Given a control polygon and set of indices, return the influence weight for
#' the specified internal knots and generate graphics showing the original,
#' coarsened, and approximated control polygons.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param x a \code{cpr_cp} object
#' @param indices an integer vector specifying the elements of \code{attr(x,
#' "knots")} to assess.  Defaults to all internal knots.
#' @param ... Additional arguments passed to \code{\link{influence_weights}}.
#'
#' @return A \code{cpr_influence_of} object.  This is a list with the following
#' elements:
#' \describe{
#'   \item{weight}{A \code{data.frame} showing the influence weight and
#'   relative rank of each internal knot}
#'   \item{orig_cp}{The original control polygon}
#'   \item{indices}{The indices of the internal knots assessed.}
#'   \item{coarsened_cp}{A list of control polygons.  These are the control
#'   polygons built on the coarsened knot sequence}
#'  \item{reinserted_cp}{A list of control polygons.  These are the control
#'  polygons resulting from the reinsertion of the assessed internal knot.}
#' }
#'
#' @seealso \code{\link{plot.cpr_influence_of}}, \code{\link{cp}}
#'
#' @export
influence_of <- function(x, indices, ...) {
  UseMethod("influence_of")
}

#' @export
influence_of.cpr_cp <- function(x, indices, ...) {
  valid_indices <- which(x$xi %in% x$iknots)

  if (missing(indices)) {
    indices <- valid_indices
  }
  
  if (length(valid_indices) < 1L) {
    message("No internal knots to assess.")
    return(invisible()) 
  }


  if (!all(indices %in% valid_indices)) {
    message(paste("interior knots have indices:", paste(which(x$xi %in% x$iknots), collapse = ", ")))

    if (any(indices < 1 | indices > length(x$xi))) {
      stop("Invalid indices.", call. = FALSE)
    }

    if (any(x$xi[indices] %in% x$bknots)) {
      stop("Assessing the influence of a boundary knot is inadvisable.",
           call. = FALSE)
    } 
  }

  xi_to_assess <- as.list(x$xi[indices])
  orig_bmat <- bsplines(x = range(x$xi), iknots = x$iknots, bknots = x$bknots, order = x$order)

  coarsened_xi <- lapply(indices, function(i) x$xi[-i])
  coarsened_iknots <- lapply(coarsened_xi, function(xi) x$iknots[x$iknots %in% xi])
  coarsened_theta <- Map(coarsen_ordinate,
                            x = xi_to_assess,
                            xi = coarsened_xi, 
                            MoreArgs = list(theta = x$cp$theta, 
                                            order = x$order))

  coarsened_bmat <- Map(bsplines,
                        iknots = coarsened_iknots,
                        MoreArgs = list(x = range(x$xi),
                                        bknots = x$bknots,
                                        order = x$order)) 
  coarsened_cp <- Map(cp, x = coarsened_bmat, theta = coarsened_theta)

  reinserted_theta <- Map(hat_ordinate,
                          x = xi_to_assess,
                          xi = coarsened_xi, 
                          MoreArgs = list(theta = x$cp$theta, 
                                          order = x$order))
  reinserted_cp <- Map(cp, theta = reinserted_theta,
                       MoreArgs = list(x = orig_bmat))

  weight <- data.frame(index = valid_indices)
  weight <- cbind(weight, influence_weights(x, ...))

  weight <- subset(weight, weight$index %in% indices)
  weight$rank <- rank(weight$w)

  out <- list(weight = weight, 
              orig_cp = x,
              indices = indices,
              coarsened_cp = coarsened_cp,
              reinserted_cp = reinserted_cp)
  class(out) <- "cpr_influence_of"

  out
} 

#' @export
print.cpr_influence_of <- function(x, ...) {
  print(x$weight, ...)
}

#' Plotting for cpr_influence_of objects
#'
#' Plot method for cpr_influence_of objects
#'
#' @param x a \code{cpr_influence_of} object
#' @param ... Arguments passed to \code{\link{plot.cpr_cp}}
#'
#' @return a \code{ggplot2} graphic
#'
#' @seealso \code{\link{influence_of}}
#'
#' @method plot cpr_influence_of
#' @export
plot.cpr_influence_of <- function(x, ...) {
  Original <- x$orig_cp
  plots <-
    lapply(seq_along(x$indices),
           function(i) {
             Coarsened <- x$coarsened_cp[[i]]
             Reinserted <- x$reinserted_cp[[i]]
             plot(Original, Coarsened, Reinserted, ...)
           })
  .data <- lapply(plots, getElement, name = "data")

  .data <- dplyr::bind_rows(.data, .id = "index")
  .data$index <- factor(.data$index, 
                        levels = seq_along(x$indices),
                        labels = sapply(x$weight$index,
                                        function(i) {
                                          bquote(xi[.(i)])
                                        }))

  ggplot2::`%+%`(plots[[1]], .data) +
  ggplot2::facet_wrap( ~ index, labeller = ggplot2::label_parsed)
}

