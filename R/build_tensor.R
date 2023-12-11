#' Build Tensor
#'
#' Tensor products of Matrices.
#'
#' @param x a matrix, or list of numeric matrices, build the tensor product
#' @param ... additional numeric matrices to build the tensor product
#'
#' @seealso
#' \code{vignette("cpr-pkg", package = "cpr")} for details on tensor products.
#'
#' @return
#' A matrix
#'
#' @examples
#'
#' A <- matrix(1:4, nrow = 10, ncol = 20)
#' B <- matrix(1:6, nrow = 10, ncol = 6)
#'
#' # Two ways of building the same tensor product
#' tensor1 <- build_tensor(A, B)
#' tensor2 <- build_tensor(list(A, B))
#' all.equal(tensor1, tensor2)
#'
#' # a three matrix tensor product
#' tensor3 <- build_tensor(A, B, B)
#' str(tensor3)
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
    build_tensor.list(c(list(tensor_product(x[[1]], x[[2]])), x[-(1:2)]))
  }
}

#' @export
build_tensor.matrix <- function(x, ...) {
  m <- c(list(x), list(...))
  build_tensor.list(c(list(tensor_product(m[[1]], m[[2]])), m[-(1:2)]))
}
