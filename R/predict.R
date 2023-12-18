#' Model Prediction
#'
#' Model prediction for \code{cpr_cp} and \code{cpr_cn} objects.
#'
#' @param object a \code{cpr_cp} or \code{cpr_cn} object
#' @param ... passed to \code{\link[stats]{predict}}
#'
#' @examples
#'
#' @export
predict.cpr_cp <- function(object, ...) {

  if (is.null(object[["fit"]])) {
    stop(sprintf("%s[['fit']] is NULL.  Set `keep_fit = TRUE` in cp call and try again.", deparse(substitute(object))))
  }

  cl <- as.list(match.call())[-1]

  dots <- list(...)

  if ("newdata" %in% names(cl)) {
    f_for_use <- data_for_use <- NULL

    do.call(generate_cp_formula_data,
            list(
                   f = as.list(object$call)[["formula"]]
                 , data = dots[["newdata"]]
                 , formula_only = FALSE
            )
    )

    cl[["object"]] <- object[["fit"]]
    cl[["newdata"]] <- data_for_use
    print(cl)

  } else {
    cl[["object"]] <- object[["fit"]]
  }

  do.call(what = stats::predict, args = cl)
}

#' @export
predict.cpr_cn <- predict.cpr_cp


updatebsplines <- function(form, nik, nbk, nord) {
  rr <- function(x, nik, nbk, nord) {
    if(is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
      x$df <- NULL
      x$iknots <- nik
      x$bknots <- nbk
      x$order  <- nord
      x
    } else if (is.recursive(x)) {
      as.call(lapply(as.list(x), rr, nik, nbk, nord))
    } else {
      x
    }
  }

  z <- lapply(as.list(form), rr, nik, nbk, nord)
  z <- eval(as.call(z))
  environment(z) <- environment(form)
  z
}

