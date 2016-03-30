#' tensor
#'
#' Tensor product of B-splines.
#'
#' Construction of the B-splines and the tensor product thereof.
#' 
#' The return form this function is the tensor product of the B-splines
#' transformations for the given variables.  Say we have variables X, Y, and Z
#' to build the tensor product of.  The columns of the returned matrix
#' correspond to the column products of the three B-splines thusly:
#'
#' x1y1z1 x2y1z1 x3y1z1 x4y1z1 x1y2z1 x2y2z1 ... x4y4z4
#'
#' for three fourth order B-splines with no internal knots.  The columns of X
#' cycle the quickest, followed by Y, and then Z.  This would be the same result
#' as
#' \code{ model.matrix( ~ bsplines(X) : bsplines(Y) : bsplines(Z) + 0) }.
#'
#' @param x a list of variables to build B-spline transforms of.  The tensor
#' product of these B-splines will be returned.
#' @param iknots a list of internal knots for each x.  If omitted, the default
#' is to place no interknal knots for all x.  If specified, the list needs to
#' contain the internal knots for all x.
#' @param bknots a list of boundary knots for each x.  As with the iknots, if
#' omitted the default will be to use the range of each x.  If specificed, the
#' use must specify the bknots for each x.
#' @param order  a list of the order for each x; defaults to 4L for all x.
#'
#' @return
#' A matrix with a class cpr_tensor
#'
#' @examples
#' tp <- with(mtcars, 
#'            tensor(x = list(disp, hp, mpg),
#'                   iknots = list(numeric(0), c(100, 150), numeric(0))) 
#'            )
#' tp
#' str(tp)
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
#'
#' @export
tensor <- function(x, iknots, bknots, orders) { 

  if (missing(iknots)) { 
    iknots <- replicate(length(x), numeric(0), simplify = FALSE)
  }
  if (missing(bknots)) { 
    bknots <- lapply(x, range)
  }
  if (missing(orders)) { 
    orders <- replicate(length(x), 4L, simplify = FALSE)
  }

  if (any(c(length(iknots), length(bknots), length(orders)) != length(x))) { 
    stop("Length of x, iknots, bknots, and orders must be the same.")
  }

  bspline_list <- mapply(FUN = cpr::bsplines,
                         x = x,
                         iknots = iknots, 
                         bknots = bknots,
                         order = orders,
                         SIMPLIFY = FALSE) 
  mats <- length(x)
  cols <- unlist(lapply(bspline_list, ncol))
  rows <- nrow(bspline_list[[1]])

  indices <- do.call(expand.grid, lapply(cols, function(x) seq(1, x, by = 1)))

  M <- lapply(1:mats, function(i) { bspline_list[[i]][, indices[, i]]})
  M <- array(unlist(M), dim = c(rows, prod(cols), mats))
  M <- apply(M, 1:2, prod)

  attr(M, "x")      = x
  attr(M, "iknots") = iknots
  attr(M, "bknots") = bknots 
  attr(M, "orders") = setNames(orders, names(x))

  class(M) <- c("cpr_tensor", class(M))
  M
}

#' @method print cpr_tensor
#' @export
print.cpr_tensor <- function(x, ...) { 
  cat("Tensor Product Matrix dims: [", paste(format(dim(x), big.mark = ",", trim = TRUE), collapse = " x "), "]\n\n", sep = "") 
  str(x, max.level = 1)
}
