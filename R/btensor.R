#' btensor
#'
#' Tensor products of B-splines.
#'
#' The return form this function is the tensor product of the B-splines
#' transformations for the given variables.  Say we have variables X, Y, and Z
#' to build the tensor product of.  The columns of the returned matrix
#' correspond to the column products of the three B-splines:
#'
#' x1y1z1 x2y1z1 x3y1z1 x4y1z1 x1y2z1 x2y2z1 ... x4y4z4
#'
#' for three fourth order B-splines with no internal knots.  The columns of X
#' cycle the quickest, followed by Y, and then Z.  This would be the same result
#' as
#' \code{ model.matrix( ~ bsplines(X) : bsplines(Y) : bsplines(Z) + 0) }.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param x a list of variables to build B-spline transforms of.  The tensor
#' product of these B-splines will be returned.
#' @param df degrees of freedom.  a list of the degrees of freedom for each
#' marginal.
#' @param iknots a list of internal knots for each x.  If omitted, the default
#' is to place no internal knots for all x.  If specified, the list needs to
#' contain the internal knots for all x.  If \code{df} and \code{iknots} are
#' both given, the \code{df} will take precedence.
#' @param bknots a list of boundary knots for each x.  As with the iknots, if
#' omitted the default will be to use the range of each x.  If specified, the
#' use must specify the bknots for each x.
#' @param order  a list of the order for each x; defaults to 4L for all x.
#'
#' @return
#' A matrix with a class cpr_bt
#'
#' @examples
#' tp <- with(mtcars,
#'            btensor(x = list(disp, hp, mpg),
#'                    iknots = list(numeric(0), c(100, 150), numeric(0)))
#'            )
#' tp
#' utils::str(tp)
#'
#' ## not run
#' # The equivalent matrix is could be generated as follows
#' tp2 <- model.matrix( ~ splines::bs(disp, intercept = TRUE) :
#'                        splines::bs(hp, knots = c(100, 150), intercept = TRUE) :
#'                        splines::bs(mpg, intercept = TRUE) + 0,
#'                     data = mtcars)
#'
#' all.equal(tp2, unclass(tp), check.attributes = FALSE)
#'
#' @export
btensor <- function(x, df = NULL, iknots = NULL, bknots, order) {

  if (!is.list(x)) { 
    warning("wrapping x into a list.")
    x <- list(x)
  }

  if (missing(bknots)) {
    bknots <- lapply(x, range)
  }

  if (missing(order)) {
    order <- as.list(rep(4L, length(x)))
  }

  if (is.null(df) & is.null(iknots)) {
    iknots <- replicate(length(x), numeric(0), simplify = FALSE)
  } else if (is.null(iknots) & !is.null(df)) { 
    iknots <- 
      mapply(function(xx, dd, oo) { 
               if (dd < oo) {
                 warning("df being set to order") 
                 numeric(0)
               } else if (dd == oo) {
                 numeric(0)
               } else {
                 trimmed_quantile(xx, probs = seq(1, dd - oo, by = 1) / (dd - oo + 1))
               }
             },
             xx = x, dd = df, oo = order, SIMPLIFY = FALSE)
  } else if (!is.null(iknots) & !is.null(df)) {
    warning("Both iknots and df defined, using iknots")
  } 

  if (any(c(length(iknots), length(bknots), length(order)) != length(x))) {
    stop("Length of x, iknots, bknots, and order must be the same.")
  }

  bspline_list <- Map(f = bsplines,
                      x = x,
                      iknots = iknots,
                      bknots = bknots,
                      order = order)
                         
  M <- build_tensor(bspline_list)

  attr(M, "bspline_list") = bspline_list
  attr(M, "call") <- match.call()
  attr(M, "environment") <- parent.frame()

  class(M) <- c("cpr_bt", class(M))
  M
}

#' @method print cpr_bt
#' @export
print.cpr_bt <- function(x, ...) {
  cat("Tensor Product Matrix dims: [", paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "), "]\n\n", sep = "")
  utils::str(x, max.level = 1)
}
