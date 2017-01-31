#' Build Tensor
#'
#' Tensor products of Matrices.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param x a matrix, or list of matrices, build the TP
#' @param ... Matrices to build the TP
#'
#' @return
#' A matrix 
#'
#' @examples
#' A <- matrix(1:4, nrow = 10, ncol = 20)
#' B <- matrix(1:6, nrow = 10, ncol = 6)
#' build_tensor(A, B)
#' build_tensor(list(A, B))
#' build_tensor(A, B, B)
#' 
#' @export
build_tensor <- function(x, ...) { 
  UseMethod("build_tensor")
}

#' @export
build_tensor.list <- function(x, ...) {
  if (length(x) == 1) {
    return(x[[1]])
  } else  {
    build_tensor.list(c(list(tp__impl(x[[1]], x[[2]])), x[-(1:2)]))
  }
}

#' @export
build_tensor.matrix <- function(x, ...) {
  m <- c(list(x), list(...))
  build_tensor.list(c(list(tp__impl(m[[1]], m[[2]])), m[-(1:2)]))
}
