#' Summarize Control Net Reduction Objects
#'
#' @param object a \code{cpr_cnr} object
#' @param ... pass through
#'
#' @return a \code{cpr_summary_cpr_cnr} object, that is just a \code{data.frame}
#'
#' @examples
#'
#' acn <- cn(log10(pdg) ~ btensor(list(day, age)
#'                                , df = list(10, 8)
#'                                , bknots = list(c(-1, 1), c(44, 53)))
#'          , data = spdg)
#' cnr0 <- cnr(acn)
#' cnr0
#' summary(cnr0)
#'
#' @export
summary.cpr_cnr <- function(object, ...) {
  rtn <- lapply(object, summary)
  for (i in seq_along(rtn)) {
    rtn[[i]]$index <- as.integer(i)
  }
  rtn <- do.call(rbind, rtn)
  class(rtn) <- c("cpr_summary_cpr_cnr", class(rtn))
  rtn
}
