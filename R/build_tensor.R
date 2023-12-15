#' Build Tensor
#'
#' Tensor products of Matrices.
#'
#' @param x a matrix
#' @param y a matrix
#' @param ... additional numeric matrices to build the tensor product
#'
#' @return a matrix
#'
#' @seealso
#' \code{vignette("cnr", package = "cpr")} for details on tensor products.
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
#' tensor2 <- do.call(build_tensor, list(A, B))
#' all.equal(tensor1, tensor2)
#'
#' # a three matrix tensor product
#' tensor3 <- build_tensor(A, B, B)
#' str(tensor3)
#'
#' @export
build_tensor <- function(x = NULL, y = NULL, ...) {
  UseMethod("build_tensor")
}

#' @export
build_tensor.matrix <- function(x = NULL, y = NULL, ...) {
  matrices <- c(as.list(environment()), list(...))
  matrices <- Filter(f = function(x) !is.null(x), matrices)

  if (!all(sapply(matrices, inherits, "matrix"))) {
    stop("All arguments passed to build_tensor need to be matrices.")
  }

  if (length(matrices) == 1) {
    return(matrices[[1]])
  }

  tp <- tensor_product(matrices[[1]], matrices[[2]])

  if (length(matrices) == 2L) {
    return(tp)
  }

  a <- list(x = tp, y = matrices[[3]])

  if (length(matrices) >= 4L) {
    for( i in seq(4, length(matrices), by = 1)) {
      a <- c(a, list(matrices[[i]]))
    }
  }

  do.call(what = build_tensor, args = a)
}
