#' Summarize Control Net Reduction Objects
#'
#' @param object a \code{cpr_cnr} object
#' @param ... pass through
#'
#' @return
#'
#' @examples
#'
#' @export
summary.cpr_cnr <- function(object, ...) {
  rtn <- lapply(object, summary)
  for (i in seq_along(rtn)) {
    rtn[[i]]$index <- as.integer(i)
  }
  do.call(rbind, rtn)
}
