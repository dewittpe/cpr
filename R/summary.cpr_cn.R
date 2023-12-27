#' Summary of Control Net
#'
#' Generate a summary of control net object
#'
#' @param object a \code{cpr_cn} object
#' @param ... pass through
#'
#' @return a \code{data.frame}
#'
#' @examples
#'
#' acn <- cn(log10(pdg) ~ btensor(list(day, age)
#'                                , df = list(10, 8)
#'                                , bknots = list(c(-1, 1), c(44, 53)))
#'          , data = spdg)
#'
#' summary(acn)
#'
#' @export
summary.cpr_cn <- function(object, ...) {
  iknots <- lapply(object$bspline_list, attr, which = "iknots")
  names(iknots) <- paste0("iknots", seq_along(iknots))

  out <-
    data.frame(dfs        = length(object$cn$theta),
               loglik     = object$loglik,
               rss        = object$rss,
               rse        = object$rse )

  for(i in seq_along(iknots)) {
    nm <- names(iknots)[i]
    out[[paste0("n_", nm)]] <- length(iknots[[i]])
    out[[nm]] <- I(iknots[i])
  }

  out

}
