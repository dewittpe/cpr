#' Build Tensor
#'
#' Tensor products of Matrices.
#'
#' @param x a matrix, or list of numeric matrices, build the tensor product
#' @param ... additional numeric matrices to build the tensor product
#'
#' @return
#' A matrix 
#'
#' @example examples/build_tensor.R
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
