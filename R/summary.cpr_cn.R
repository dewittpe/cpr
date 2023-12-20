#' @export
#' @param object a \code{cpr_cn} object
#' @rdname cn
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
